#include <fcntl.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include "z80/z80.h"

#define STDIN_DATA 0
#define STDOUT_DATA 1
#define STDERR_DATA 2
#define SYS_EXIT 3

z80 cpu;
bool timing;
long nb_instructions;

#define MEMORY_SIZE 0x10000
uint8_t memory[MEMORY_SIZE];

uint8_t rb(void *userdata, uint16_t addr) {
  return memory[addr];
}

void wb(void *userdata, uint16_t addr, uint8_t val) {
  memory[addr] = val;
}

uint8_t in(z80 *const z, uint8_t port) {
  switch (port) {
  case STDIN_DATA:
    {
      uint8_t data = 4; // EOT
      if (read(STDIN_FILENO, &data, 1) == -1) {
        perror("read()");
        exit(EXIT_FAILURE);
      }
      return data;
    }
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
