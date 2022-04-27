#include <fcntl.h>
#include <poll.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "z80/z80.h"

#define STDIN_DATA 0
#define STDOUT_DATA 1
#define STDERR_DATA 2
#define STDIN_STATUS 3
#define STDOUT_STATUS 4
#define STDERR_STATUS 5
#define USLEEP_LO 6
#define USLEEP_HI 7
#define SYS_EXIT 8

z80 cpu;
bool timing;
long nb_instructions;

int stdin_fd;
int g_argn = 0;
int g_argc = 1;
const char *default_argv = "-";
const char **g_argv = &default_argv;

#define MEMORY_SIZE 0x10000
uint8_t memory[MEMORY_SIZE];
uint8_t usleep_lo;

uint8_t rb(void *userdata, uint16_t addr) {
  return memory[addr];
}

void wb(void *userdata, uint16_t addr, uint8_t val) {
  memory[addr] = val;
}

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

uint8_t in(z80 *const z, uint8_t port) {
  switch (port) {
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

void out(z80 *const z, uint8_t port, uint8_t val) {
  switch (port) {
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
    if (timing)
      fprintf(
        stderr,
        "%lu instructions executed on %lu cycles\n",
        nb_instructions,
        cpu.cyc
      );
    exit(val);
  }
}

int main(int argc, char **argv) {
  int argn = 1;
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
  if (read(fd, memory, MEMORY_SIZE) == -1) {
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

  z80_init(&cpu);
  cpu.read_byte = rb;
  cpu.write_byte = wb;
  cpu.port_in = in;
  cpu.port_out = out;

  while (true) {
    ++nb_instructions;

    // warning: the following line will output dozens of GB of data.
    //z80_debug_output(&cpu);

    z80_step(&cpu);
  }
}