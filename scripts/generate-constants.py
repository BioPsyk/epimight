#!/usr/bin/env python3

import os
import os.path
import logging
import argparse
import sys
import psycopg2
import psycopg2.extras
import subprocess

from contextlib import contextmanager
from jinja2 import Environment, FileSystemLoader

#-------------------------------------------------------------------------------
# Constants

SCRIPT_NAME        = 'generate-constants'
PROJECT_DIR        = os.path.realpath(os.path.dirname(os.path.dirname(__file__)))
TMP_DIR            = os.path.join(PROJECT_DIR, "tmp")
TEMPLATE_DIR       = os.path.join(PROJECT_DIR, "inst", "extdata", "R")
RELATIONSHIP_KINDS = {
  "1C":     0.125,
  "1C1R":   0.0625,
  "1C2R":   0.03125,
  "1C3R":   0.015625,
  "1G":     0.25,
  "1GAv":   0.125,
  "1GHAv":  0.0625,
  "2C":     0.03125,
  "2C1R":   0.015625,
  "2C2R":   0.0078125,
  "2G":     0.125,
  "2GHAv":  0.03125,
  "2GAv":   0.0625,
  "3C":     0.0078125,
  "3C1R":   0.00390625,
  "3G":     0.0625,
  "3GAv":   0.03125,
  "4C":     0.001953125,
  "4G":     0.03125,
  "Av":     0.25,
  "FS":     0.5,
  "H1C":    0.0625,
  "H1C1R":  0.03125,
  "H1C2R":  0.015625,
  "H2C":    0.015625,
  "H2C1R":  0.0078125,
  "HAv":    0.125,
  "HS":     0.25,
  "mHS":    0.25,
  "pHS":    0.25,
  "PO":     0.5
}
VERTICAL_RELATIONSHIP_KINDS = [
  "PO",
  "1G",
  "2G",
  "3G",
  "4G",
  "Av",
  "1GAv",
  "2GAv",
  "3GAv",
]

#-------------------------------------------------------------------------------
# Logger setup

logger = logging.getLogger(SCRIPT_NAME)
logger.setLevel(logging.DEBUG)

basic_formatter = logging.Formatter(
  '[%(levelname)s] %(message)s'
)

stream_handler = logging.StreamHandler()
stream_handler.setLevel(logging.INFO)

stream_handler.setFormatter(basic_formatter)
logger.addHandler(stream_handler)

#-------------------------------------------------------------------------------

def run_enum_query(enum_type, db_cur, db_conn):
  db_cur.execute(f"SELECT unnest(enum_range(NULL::{enum_type}))::text")

  rows = db_cur.fetchall()

  db_conn.commit()

  return [row[0] for row in rows]

@contextmanager
def db_connection():
  try:
    db_conn = psycopg2.connect(
      host     = "localhost",
      dbname   = "ibp_registry",
      user     = "postgres",
      password = "devpass"
    )

    db_cur = db_conn.cursor(
      cursor_factory=psycopg2.extras.DictCursor
    )

    yield (db_conn, db_cur)
  except:
    raise
  finally:
    db_cur.close()
    db_conn.close()

def main(args):
  logger.info('Connecting to database')

  with db_connection() as (db_conn, db_cur):
    env = Environment(
      loader = FileSystemLoader([TEMPLATE_DIR])
    )

    template = env.get_template("constants.R")
    r_code = template.render(
      relationship_kinds          = RELATIONSHIP_KINDS,
      vertical_relationship_kinds = VERTICAL_RELATIONSHIP_KINDS,
      civil_statuses              = run_enum_query("civil.status", db_cur, db_conn),
      diagnosis_kinds             = run_enum_query("medical.diagnosis_kind", db_cur, db_conn),
      genders                     = run_enum_query("civil.gender", db_cur, db_conn)
    )

  target_path = os.path.join(TMP_DIR, "constants.R")

  with open(target_path, "w") as f:
    f.write(r_code)

  logger.info("Generated R code written to: %s", target_path)

  subprocess.run(["Rscript", target_path], check=True)

#-------------------------------------------------------------------------------

if __name__ == '__main__':
  parser     = argparse.ArgumentParser(prog=SCRIPT_NAME)
  subparsers = parser.add_subparsers()

  parser.add_argument(
    '--log_level',
    type=str,
    choices=['error', 'info', 'debug'],
    help='Controls the log level, "info" is default'
  )

  args = parser.parse_args()

  if args.log_level == 'debug':
    stream_handler.setLevel(logging.DEBUG)
  elif args.log_level == 'error':
    stream_handler.setLevel(logging.ERROR)

  main(args)
