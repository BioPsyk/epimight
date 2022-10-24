from behave import *

import argparse
import logging
import os
import os.path
import sys
import time
import subprocess
import csv

from jinja2 import Environment, FileSystemLoader

#-------------------------------------------------------------------------------
# Constants

PROJECT_DIR = os.path.realpath(
  os.path.join(__file__, "../../../../")
)
TMP_DIR = os.path.join(PROJECT_DIR, "tmp")

#-------------------------------------------------------------------------------
# Steps

@when('I retrieve TTE for "{target_regexp}" with "{excl_regexp}" excluded')
def step_impl(context, target_regexp, excl_regexp):
  script_path = os.path.join(PROJECT_DIR, "inst", "extdata", "R", "two-disorders-exclusion.R")
  output_path = os.path.join(TMP_DIR, "test_output.csv")
  query_path  = os.path.join(TMP_DIR, "test_output.csv.sql")

  subprocess.run(
    ["Rscript", script_path, target_regexp, excl_regexp],
    cwd=PROJECT_DIR,
    #capture_output=True,
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

@when('I retrieve TTE for "{target_regexp}" with "{excl_regexp}" excluded with "{relationship_kind}" relatives')
def step_impl(context, target_regexp, excl_regexp, relationship_kind):
  script_path = os.path.join(PROJECT_DIR, "inst", "extdata", "R", "two-disorders-exclusion-with-relatives.R")
  output_path = os.path.join(TMP_DIR, "test_output.csv")
  query_path  = os.path.join(TMP_DIR, "test_output.csv.sql")

  subprocess.run(
    ["Rscript", script_path, target_regexp, excl_regexp, relationship_kind],
    cwd=PROJECT_DIR,
    #capture_output=True,
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
