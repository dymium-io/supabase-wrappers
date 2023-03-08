#!/usr/bin/python
# -*- coding: utf-8 -*-

import psycopg2
import sys
import random
import string 
import argparse

parser = argparse.ArgumentParser()

parser.add_argument("--stage",  
                    choices=["direct", "connector", "dbguardian", "client"], help="Select which stage to measure", required=True)
parser.add_argument("--port", help="Port to connect", required=False)
parser.add_argument( "--pass", help="Password", required=False)
parser.add_argument("--user", help="User", required=False)
parser.add_argument( "--chunk", help="Chunk size", required=False)
parser.add_argument("--limit", help="Limit", required=False)

args = parser.parse_args()


dport = 0
if(args.port != None):
    dport = int(args.port)

chunk=10
if(args.chunk != None):
    chunk = int(args.chunk)

if(args.stage == "direct"):
    if(dport == 0):
        port = 5432
    con = psycopg2.connect(database='aircrafts', 
                       port=port, 
                       host="localhost", user='dymium')
elif(args.stage == "connector"):
    if(dport == 0):
        port = 30000
    con = psycopg2.connect(database='aircrafts', 
                       port=port, 
                       host="localhost", user='dymium')
elif(args.stage == "dbguardian"):
    if(dport == 0):
        port = 9090
    if(args.user == None):
        print("User must be specified")
        exit(1)
    if(args.password == None):
        print("Password must be specified")
        exit(1)
                
    con = psycopg2.connect(database='planes', 
                       port=port, 
                       host="localhost", user=args.user, password=args.password)

elif(args.stage == "dbguardian"):
    if(dport == 0):
        print("Port must be specified!")
        exit(1)
    if(args.user == None):
        print("User must be specified")
        exit(1)
    if(args.password == None):
        print("Password must be specified")
        exit(1)

    con = psycopg2.connect(database='planes', 
                       port=dport, 
                       host="localhost", user=args.user, password=args.password)
    
cur = con.cursor()
cur.itersize = chunk
if(args.limit != None) :
    cur.execute('''SELECT id,img FROM public.images limit ''' + args.limit + ''';''')
else:
    cur.execute('''SELECT id,img FROM public.images;''')
counter = 0
while True:
    rows = cur.fetchmany(chunk)

    if not rows:
        break

    for row in rows:
        #print(row[0])
        counter += 1
        pass

print("done %d records"%(counter))