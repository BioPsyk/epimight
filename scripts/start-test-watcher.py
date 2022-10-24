#!/usr/bin/env python3

import argparse
import logging
import os
import os.path
import sys
import time
import subprocess

from watchdog.observers import Observer
from watchdog.events    import PatternMatchingEventHandler

#-------------------------------------------------------------------------------
# Constants

SCRIPT_NAME = "start-test-watcher"
PROJECT_DIR = os.path.realpath(os.path.dirname(os.path.dirname(__file__)))
R_DIR       = os.path.join(PROJECT_DIR, "R")
SQL_DIR     = os.path.join(PROJECT_DIR, "inst", "extdata", "sql")
TEST_DIR    = os.path.join(PROJECT_DIR, "tests", "testthat")

#-------------------------------------------------------------------------------
# Logger setup

logger = logging.getLogger(SCRIPT_NAME)
logger.setLevel(logging.DEBUG)

basic_formatter = logging.Formatter(
    "[%(levelname)s] %(message)s"
)

stream_handler = logging.StreamHandler()
stream_handler.setLevel(logging.INFO)

stream_handler.setFormatter(basic_formatter)
logger.addHandler(stream_handler)

#-------------------------------------------------------------------------------
# Main tasks

test_queue = {}

class SQLEventHandler(PatternMatchingEventHandler):
  def __init__(self, *args, **kwargs):
    super(SQLEventHandler, self).__init__(*args, **kwargs)

  def on_modified(self, event):
    target_path = os.path.join(R_DIR, "TTERetriever-class.R")

    with open(target_path, "a"):
        os.utime(target_path)

class REventHandler(PatternMatchingEventHandler):
  def __init__(self, *args, **kwargs):
    super(REventHandler, self).__init__(*args, **kwargs)

  def on_modified(self, event):
    if event.is_directory:
      return

    file_name = os.path.basename(event.src_path)

    if file_name.startswith(".#"):
      return

    if file_name.startswith("test_"):
      test_path = event.src_path
    else:
      test_path = os.path.join(TEST_DIR, f"test_{file_name}")

    if not os.path.exists(test_path):
      return

    test_queue[test_path] = time.time()

def handle_test_queue():
  global test_queue

  timestamp = time.time()

  if not bool(test_queue):
    return

  for test_path in test_queue:
    logger.info("Running test: %s", test_path)
    subprocess.run([
      "R",
      "-e",
      f"devtools::test_active_file(file = '{test_path}', stop_on_failure = TRUE)"
    ], check=False)

  test_queue = {}

def main(args):
    logger.info("Started watching %s and %s", R_DIR, SQL_DIR)

    r_handler   = REventHandler(patterns=["*.R"])
    sql_handler = SQLEventHandler(patterns=["*.sql"])
    observer    = Observer()

    observer.schedule(r_handler, R_DIR)
    observer.schedule(r_handler, TEST_DIR)
    observer.schedule(sql_handler, SQL_DIR, recursive=True)
    observer.start()

    try:
      while True:
        handle_test_queue()
        time.sleep(0.1)
    except KeyboardInterrupt as err:
      pass
    except Exception as err:
      logger.error("Observer failed with", err)

    observer.stop()
    observer.join()

    logger.info("Watcher shutdown, bye!")

#-------------------------------------------------------------------------------

if __name__ == "__main__":
    parser     = argparse.ArgumentParser(prog=SCRIPT_NAME)
    subparsers = parser.add_subparsers()

    parser.add_argument(
        "--log_level",
        type=str,
        choices=["error", "info", "debug"],
        help="Controls the log level, 'info' is default"
    )

    args = parser.parse_args()

    if args.log_level == "debug":
        stream_handler.setLevel(logging.DEBUG)
    elif args.log_level == "error":
        stream_handler.setLevel(logging.ERROR)

    main(args)
