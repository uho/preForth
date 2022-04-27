#include <fcntl.h>
#include <poll.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "vrEmu6502/src/vrEmu6502.h"

#define IO_PAGE 0x200
#define STDIN_DATA 0x200
#define STDOUT_DATA 0x201
#define STDERR_DATA 0x202
#define STDIN_STATUS 0x203
#define STDOUT_STATUS 0x204
#define STDERR_STATUS 0x205
#define USLEEP_LO 0x206
#define USLEEP_HI 0x207
#define SYS_EXIT 0x208

#define LOAD_ADDRESS 0x300
#define RESET_VECTOR 0xfffc

VrEmu6502 *cpu;

int stdin_fd;
int g_argn = 0;
int g_argc = 1;
const char *default_argv = "-";
const char **g_argv = &default_argv;

#define MEMORY_SIZE 0x10000
uint8_t memory[MEMORY_SIZE];
uint8_t usleep_lo;
int exit_flag;

// call with g_argn < g_argc
void open_stdin(void) {
  if (strcmp(g_argv[g_argn], "-") == 0)
    stdin_fd = STDIN_FILENO;
  else {
    stdin_fd = open(g_argv[g_argn], O_RDONLY);
    if (stdin_fd == -1) {
      perror(g_argv[g_argn]);
      exit(EXIT_FAILURE);
    }
  }
}

void close_stdin(void) {
  if (stdin_fd != STDIN_FILENO)
    close(stdin_fd);
}

uint8_t mem_read(uint16_t addr, bool isDbg) {
  if ((addr & 0xff00) != IO_PAGE)
    return memory[addr];

  switch (addr) {
  case STDIN_DATA:
    {
      uint8_t data = 4; // EOT
      if (g_argn < g_argc)
        while (true) {
          ssize_t count = read(stdin_fd, &data, 1);
          if (count == -1) {
            perror("read()");
            exit(EXIT_FAILURE);
          }
          if (count)
            break;
          close_stdin();
          ++g_argn;
          if (g_argn >= g_argc)
            break;
          open_stdin();
        }
      return data;
    }
  case STDIN_STATUS:
    {
      if (g_argn >= g_argc)
        return 1; // if no more input, force application to read EOT
      struct pollfd fd = {stdin_fd, POLLIN, 0};
      if (poll(&fd, 1, 0) == -1) {
        perror("poll()");
        exit(EXIT_FAILURE);
      }
      return (fd.revents & POLLIN) != 0;
    }
  case STDOUT_STATUS:
    {
      struct pollfd fd = {STDOUT_FILENO, POLLOUT, 0};
      if (poll(&fd, 1, 0) == -1) {
        perror("poll()");
        exit(EXIT_FAILURE);
      }
      return (fd.revents & POLLOUT) != 0;
    }
  case STDERR_STATUS:
    {
      struct pollfd fd = {STDERR_FILENO, POLLOUT, 0};
      if (poll(&fd, 1, 0) == -1) {
        perror("poll()");
        exit(EXIT_FAILURE);
      }
      return (fd.revents & POLLOUT) != 0;
    }
  case USLEEP_LO:
    return usleep_lo;
  }
  return 0xff;
}

void mem_write(uint16_t addr, uint8_t val) {
  if ((addr & 0xff00) != IO_PAGE) {
    memory[addr] = val;
    return;
  }

  switch (addr) {
  case STDOUT_DATA:
    if (write(STDOUT_FILENO, &val, 1) == -1) {
      perror("write()");
      exit(EXIT_FAILURE);
    }
    break;
  case STDERR_DATA:
    if (write(STDERR_FILENO, &val, 1) == -1) {
      perror("write()");
      exit(EXIT_FAILURE);
    }
    break;
  case USLEEP_LO:
    usleep_lo = val;
    break;
  case USLEEP_HI:
    usleep(usleep_lo | (val << 8));
    break;
  case SYS_EXIT:
    exit_flag = val | 0x100;
    break;
  }
}

int main(int argc, char **argv) {
  int argn = 1;
  bool timing = false;
  if (argn < argc && strcmp(argv[argn], "-t") == 0) {
    timing = true;
    ++argn;
  }

  if (argn >= argc) {
    printf("usage: %s [-t] program.bin\n", argv[0]);
    exit(EXIT_FAILURE);
  }

  int fd = open(argv[argn], O_RDONLY);
  if (fd == -1) {
    perror(argv[argn]);
    exit(EXIT_FAILURE);
  }
  if (read(fd, memory + LOAD_ADDRESS, MEMORY_SIZE - LOAD_ADDRESS) == -1) {
    perror("read()");
    exit(EXIT_FAILURE);
  }
  close(fd);

  // implement "cat" functionality for stdin
  // if not enough arguments, supply default argument of "-"
  ++argn;
  if (argn < argc) {
    g_argn = argn;
    g_argc = argc;
    g_argv = (const char **)argv;
  }
  open_stdin();

  // do this before creating the CPU
  memory[RESET_VECTOR] = (uint8_t)(LOAD_ADDRESS & 0xff);
  memory[RESET_VECTOR + 1] = (uint8_t)(LOAD_ADDRESS >> 8);

  cpu = vrEmu6502New(CPU_65C02, mem_read, mem_write);
  if (cpu == NULL) {
    perror("malloc()");
    exit(EXIT_FAILURE);
  }

  long nb_ticks = 0;
  while (!exit_flag) {
    vrEmu6502Tick(cpu);
    ++nb_ticks;
  }

  vrEmu6502Destroy(cpu);

  if (timing)
    fprintf(
      stderr,
      "%lu ticks executed\n",
      nb_ticks
    );
  exit(exit_flag & 0xff);
}
