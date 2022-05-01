#!/usr/bin/env python3

import bisect
import sys

EXIT_SUCCESS = 0
EXIT_FAILURE = 1

if len(sys.argv) < 2:
  print(f'usage: {sys.argv[0]:s} program.rst <trace.txt >annotated_trace.txt')
  sys.exit(EXIT_FAILURE)

symbol_table = []
with open(sys.argv[1]) as fin:
  i = 0 # fallback sort key (use symbol defined later in file)
  for line in fin:
    fields = line[32:].split()
    if (
      len(fields) and
      fields[0][-1:] == ':' and
      fields[0][:1] != ';' and
      fields[0][-2:] != '$:'
    ):
      addr = int(line[3:7], 16)
      symbol = fields[0][:-1]
      symbol_table.append((addr, i, symbol))
      i += 1
symbol_table.sort()
symbol_table = (
  [addr for addr, _, _ in symbol_table],
  [symbol for _, _, symbol in symbol_table]
)

for line in sys.stdin:
  fields = line.split('=')
  for i in range(1, len(fields)):
    addr = int(fields[i][:4], 16)
    j = bisect.bisect_right(symbol_table[0], addr)
    if j:
      j -= 1
      offset = addr - symbol_table[0][j]
      symbol = symbol_table[1][j]
      fields[i] = fields[i][:4] + f'({symbol:s}+{offset:04x})' + fields[i][4:]
  sys.stdout.write('='.join(fields))
