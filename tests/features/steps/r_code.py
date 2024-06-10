from behave import *

import argparse
import csv
import json
import logging
import os
import os.path
import subprocess
import sys
import time

from jinja2 import Environment, FileSystemLoader

#-------------------------------------------------------------------------------
# Constants

PROJECT_DIR = os.path.realpath(
  os.path.join(__file__, "../../../../")
)
TMP_DIR = os.path.join(PROJECT_DIR, "tmp")

def run_r_code(context, relationship_kind=None):
  diagnosis_filters = {}

  for row in context.table:
    diagnosis_args = {}
    diagnosis_key = None

    for idx, col in enumerate(row.headings):
      if col == "key":
        diagnosis_key = row.cells[idx]
      else:
        diagnosis_args[col] = row.cells[idx]

    diagnosis_filters[diagnosis_key] = diagnosis_args

  args = {
    "samples": {
      "diagnosis_filters": diagnosis_filters
    },
    "study_end_at": "2016-12-31"
  }

  if relationship_kind is not None:
    args["relatives"] = {
      "relationship_filters": {
        "kind": relationship_kind
      }
    }

  script_path = os.path.join(PROJECT_DIR, "inst", "extdata", "R", "run-query-from-json.R")
  json_path   = os.path.join(TMP_DIR, "test_output.json")
  output_path = os.path.join(TMP_DIR, "test_output.csv")
  query_path  = os.path.join(TMP_DIR, "test_output.sql")

  with open(json_path, "w") as f:
    f.write(json.dumps(args, indent=2))

  subprocess.run(
    ["Rscript", script_path, json_path],
    cwd=PROJECT_DIR,
    capture_output=True,
    check=True
  )

  with open(output_path, "r") as f:
    reader = csv.DictReader(f, delimiter=",")

    rows = []

    for row in reader:
      rows.append(row)

    context.prev_results = rows

  with open(query_path, "r") as f:
    context.prev_query = f.read()

@when('I retrieve TTE for the following diagnoses')
def step_impl(context):
  run_r_code(context)

@when('I retrieve TTE for the following diagnoses with "{relationship_kind}" relatives')
def step_impl(context, relationship_kind):
  run_r_code(context, relationship_kind)
