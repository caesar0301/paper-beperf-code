#!/bin/env python
# View and sample the avro-format NetworkFlow data.
# chenxm

import sys, os

import avro.schema
from avro.datafile import DataFileReader, DataFileWriter
from avro.io import DatumReader, DatumWriter

def print_usage():
    print("Usage: dataReader [-bh] [-n recordNum] <avrofile>")
    print("      -b    write to output file 'sample.avro'")
    print("      -n    number of records to read")
    print("      -h    for help")

if len(sys.argv) < 2:
    print_usage()
    sys.exit(-1)

data_file = None
record_num = 1
sample = False

i = 1
while i < len(sys.argv):
    if sys.argv[i] == '-h':
        print_usage(); sys.exit(-1)
    elif sys.argv[i] == '-b':
        sample = True
    elif sys.argv[i] == '-n':
        try:
            record_num = int(sys.argv[i+1]); i += 1
        except IndexError:
            print("ERROR: option '-n' has not parameter."); sys.exit(-1)
        except ValueError:
            print("ERROR: the recordNum must be an integer."); sys.exit(-1)
    else:
        data_file = sys.argv[i]
        if not os.path.isfile(data_file):
            print("ERROR: the input must be an avro file: %s" % data_file)
            sys.exit(-1)
    i += 1

if data_file is None:
    print("ERROR: input file can not be empty")
    sys.exit(-1)


reader = DataFileReader(open(data_file, "r"), DatumReader())
sf = os.path.join(
    os.path.dirname(os.path.realpath(__file__)),
    'beperf-hadoop/doc/avro/netflow.avsc')
schema = avro.schema.parse(open(sf).read())

i = 0
writer = None
for flow in reader:
    if i < record_num:
        i+=1
        if not sample:
            print flow; continue
        if writer is None:
            writer = DataFileWriter(open("sample.avro", "w"), DatumWriter(), schema)
        writer.append(flow)
    else:
        break

reader.close()
if writer != None:
    writer.close()

print("%d records printed/sampled." % record_num)
