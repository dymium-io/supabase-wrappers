#!/usr/bin/python
# -*- coding: utf-8 -*-

import psycopg2
import sys
import random
import string 
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("--file", help="Image file", required=True)
parser.add_argument("--count", help="How many to insert", default="10000")
args = parser.parse_args()


con = psycopg2.connect(database='aircrafts', user='dymium')

f = open(args.file, "rb")
data = f.read()
f.close()
con.autocommit = True
cur = con.cursor()
count = 0
maxcount = int(args.count)
while(count < maxcount):
    id = ''.join(random.choices(string.ascii_lowercase +
                             string.digits, k=16))
    cur.execute('''insert into public.images values(%s, %s); ''', (id, data))

    cur.execute('select count(*) from public.images;')
    t = cur.fetchone()
    print('images: ', t[0])
    count = count + 1

