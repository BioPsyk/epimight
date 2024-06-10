from behave import *

#================================================================================
# Given using sets

@given('the following set of genealogical relationships')
def step_impl(context):
  for row in context.table:
    keys   = ', '.join(row.headings)
    values = ', '.join(context.db_serialize(row.cells))
    query  = """
      INSERT INTO genealogy.relationships
                  (origin, %s)
           VALUES ('test', %s)
    """ % (keys, values)

    context.db_cur.execute(query)

  context.db_conn.commit()

#================================================================================
# When

@when('I get samples by genealogical relationship kind "{kind:w}"')
def step_impl(context, kind):
  context.db_cur.execute("""
    SELECT *
      FROM genealogy.samples_by_relationship_kind(%s)
     ORDER BY (person_id);
  """, (kind,))

  context.prev_results = context.db_cur.fetchall()

  context.db_conn.commit()
