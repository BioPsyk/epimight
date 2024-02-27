import json

from tabulate import tabulate
from behave import *

def tabulate_realdict(results, table):
  if table is None:
    columns = results[0].keys()
  else:
    columns = table.headings

  rows = []

  for row in results:
    curr = []

    for col in columns:
      curr.append(row[col])

    rows.append(curr)

  print(tabulate(rows, columns, tablefmt="github"))

  for _ in rows:
    print("\n")

  print("\n")

def pretty_print_assert(f):
  def inner(context, *args, **kwargs):
    try:
      return f(context, *args, **kwargs)
    except AssertionError as e:
      print("--- Query:")
      print(context.prev_query)
      print("--- Results:")
      tabulate_realdict(context.prev_results, context.table)

      raise e

  return inner

#================================================================================
# Then

@then('the results contains {amount:d} rows')
def step_impl(context, amount):
  curr_amount = len(context.prev_results)
  assert curr_amount == amount, f'Expected {amount} rows, found {curr_amount}'

@then('all rows have the value "{value:w}" in column "{column:w}"')
def step_impl(context, column, value):
  for index, row in enumerate(context.prev_results):
    assert str(row[column]) == str(value), f'Expected all rows to have value "{value}" in column {column}, found: {row[column]}'

@then('no rows have the value "{value:w}" in column "{column:w}"')
def step_impl(context, column, value):
  for row in context.prev_results:
    assert str(row[column]) != str(value), f'Expected no rows to have "{value}" in column {column}, found: {row[column]}'

#================================================================================
# Then using sets

@then('the results contains the following rows')
@pretty_print_assert
def step_impl(context):
  row_amount  = len(context.prev_results)
  matches     = 0
  exp_matches = len(context.table.rows)

  assert row_amount == exp_matches, f'Expected {exp_matches} rows, found: {row_amount}'
  assert isinstance(context.prev_results, list), 'Expected results to be a list'

  for index, exp_row in enumerate(context.table):
    row     = context.prev_results[index]
    matched = True

    for key, val in exp_row.items():
      assert key in row, f'Column "{key}" was not found in row: {row.keys()}'

      if str(row[key]) != str(val):
        matched = False
        break

    if matched:
      matches += 1

  assert matches == exp_matches, f'Expected {exp_matches} matching rows, found {matches}'

@then('the results are empty')
def step_impl(context):
  assert len(context.prev_results) == 0
