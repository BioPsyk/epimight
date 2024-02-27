#!/usr/bin/env python3

import argparse
import json
import logging
import os
import os.path
import subprocess
import sys

import psycopg2
import psycopg2.extras
from contextlib import contextmanager

#-------------------------------------------------------------------------------
# Constants

SCRIPT_NAME = 'generate-constants'
PROJECT_DIR = os.path.realpath(os.path.dirname(os.path.dirname(__file__)))
TMP_DIR     = os.path.join(PROJECT_DIR, "tmp")
R_DIR       = os.path.join(PROJECT_DIR, "inst", "extdata", "R")

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
  "3GAv"
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

def run_enum_query(enum_type, db_cur, db_conn):
  db_cur.execute(f"SELECT unnest(enum_range(NULL::{enum_type}))::text")

  rows = db_cur.fetchall()

  db_conn.commit()

  return [row[0] for row in rows]

def mk_diagnosis_filters(diagnosis_kinds, patient_kinds):
  return {
    "type": "generic_named_list",
    "minimum_length": 1,
    "maximum_length": 10,
    "items": {
      "type": "named_list",
      "required": True,
      "description":
      """The filters to apply on each diagnosis collected for a single individual.
      From the results of that filtering, the diagnosis that occured first is selected
      as the 'time-to-event' for that individual.""",
      "properties": {
        "icd_codes_regexp": {
          "required": True,
          "type": "string",
          "description":
          """POSIX regular expression that is run on a diagnosis ICD code to determine if the
          diagnosis should be included or not. When the regexp matches, the diagnosis is
          included and when it doesn't match, the diagnosis is excluded.

          For details, see the PostgreSQL documentation:
          https://www.postgresql.org/docs/13/functions-matching.html#FUNCTIONS-POSIX-REGEXP"""
        },
        "diagnosis_kinds": {
          "type": "list",
          "description": "Which kinds of diagnoses to investigate.",
          "items": {
            "type": "string",
            "enum": diagnosis_kinds
          }
        },
        "record_origin": {
          "type": "string",
          "description":
          """Which register the medical records originates from.

          - "pcrr": Psychiatric Central Research Register
          - npr  = National Patient Register""",
          "enum": ["pcrr", "npr"]
        },
        "patient_kinds": {
          "type": "list",
          "description": "Which kind of patient the medical record is for. Inpatient, outpatient, etc.",
          "items": {
            "type": "string",
            "enum": patient_kinds
          }
        }
      }
    }
  }

def mk_individual_filters(genders, civil_statuses):
  return {
    "type": "named_list",
    "description":
    """The filters to apply on each individual in the population.
    The results of that filtering becomes the sample under study.""",
    "properties": {
      "born_at_min": {"type": "date"},
      "born_at_max": {"type": "date"},
      "gender": {
        "type": "string",
        "enum": genders
      },
      "status": {
        "type": "list",
        "items": {
          "type": "string",
          "enum": civil_statuses
        }
      },
      "custom": {
        "type": "string"
      }
    }
  }

def mk_rules(relationship_kinds, civil_statuses, diagnosis_kinds, genders, patient_kinds):
  rules = {
    "samples": {
      "type": "named_list",
      "required": True,
      "properties": {
        "diagnosis_filters": mk_diagnosis_filters(diagnosis_kinds, patient_kinds),
        "individual_filters": mk_individual_filters(genders, civil_statuses)
      }
    },
    "relatives": {
      "type": "named_list",
      "properties": {
        "relationship_filters": {
          "required": True,
          "type": "named_list",
          "required": True,
          "properties": {
            "kind": {
              "type": "string",
              "required": True,
              "enum": relationship_kinds
            },
            "component": {
              "type": "string",
              "description":
              """The genealogy is divided into two 'components' (lingo for 'group' in graph theory):

              - "pedigree1": Everyone with Danish ancestry
              - "rest": Everyone without Danish ancestry""",
              "enum": ["pedigree1", "rest"]
            }
          }
        },
        "diagnosis_filters": mk_diagnosis_filters(diagnosis_kinds, patient_kinds),
        "individual_filters": mk_individual_filters(genders, civil_statuses)
      }
    },
    "study_end_at": {
      "type": "date",
      "required": True,
      "description":
      """The study end date to use when determining the failure status/time.
      If an individual isn't diagnosed or deceased before this date, they
      have 'survived' the period, and is marked as 'censored (0)'."""
    },
    "extra_columns": {
      "type": "list",
      "minimum_length": 1,
      "items": {
        "type": "string"
      }
    }
  }

  return rules

def main(args):
  logger.info("Connecting to database")

  with db_connection() as (db_conn, db_cur):
    logger.info("Retrieving database constants")

    relationship_kinds = run_enum_query("genealogy.relationship_kind", db_cur, db_conn)
    civil_statuses     = run_enum_query("civil.status", db_cur, db_conn)
    diagnosis_kinds    = run_enum_query("medical.diagnosis_kind", db_cur, db_conn)
    genders            = run_enum_query("civil.gender", db_cur, db_conn)
    patient_kinds      = run_enum_query("medical.patient_kind", db_cur, db_conn)

  logger.info("Writing JSON files")

  rules = mk_rules(relationship_kinds, civil_statuses, diagnosis_kinds, genders, patient_kinds)
  constants = {
    "relationship_kinds": RELATIONSHIP_KINDS,
    "vertical_relationship_kinds": VERTICAL_RELATIONSHIP_KINDS,
    "civil_statuses": civil_statuses,
    "diagnosis_kinds": diagnosis_kinds,
    "genders": genders
  }

  rules["samples"]["properties"]["diagnosis_filters"]["required"] = True

  json_constants_path  = os.path.join(TMP_DIR, "constants.json")
  json_rules_path      = os.path.join(TMP_DIR, "rules.json")

  with open(json_constants_path, "w") as f:
    f.write(
      json.dumps(constants, indent=2)
    )

  with open(json_rules_path, "w") as f:
    f.write(
      json.dumps(rules, indent=2)
    )

  logger.info("Writing internal R data from JSON files")

  r_script_path = os.path.join(R_DIR, "write-internal-data.R")

  subprocess.run(
    ["Rscript", r_script_path, json_constants_path, json_rules_path],
    check=True
  )

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
