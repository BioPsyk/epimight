from behave import *

#================================================================================
# Given

@given('person {person_id:d} was born "{born_at:ti}" as "{gender:w}"')
def step_impl(context, person_id, born_at, gender):
  context.db_cur.execute("""
    INSERT INTO civil.people
                (id, born_at, gender, status)
         VALUES (%s, %s, %s, 'danish-resident')
  """, (person_id, born_at, gender))

  context.db_conn.commit()

@given('person {person_id:d} died at "{died_at:ti}"')
def step_impl(context, person_id, died_at):
  context.db_cur.execute("""
    UPDATE civil.people
       SET status_changed = %s
         , status = 'dead'
     WHERE id = %s
  """, (died_at, person_id))

  context.db_conn.commit()

#================================================================================
# Given using sets

@given('the following set of people')
def step_impl(context):
  for row in context.table:
    keys   = ', '.join(row.headings)
    values = ', '.join(context.db_serialize(row.cells))
    query  = """
      INSERT INTO civil.people
                  (%s)
           VALUES (%s)
    """ % (keys, values)

    context.db_cur.execute(query)

  context.db_conn.commit()

@given('the following set of peoples birthplaces')
def step_impl(context):
  for row in context.table:
    query  = """
      UPDATE civil.people
         SET birthplace_id = sub.id
        FROM ( SELECT *
                 FROM cpr.birthplaces
                WHERE name = '%s'
                LIMIT 1
             ) AS sub
       WHERE civil.people.id = %s
    """ % (row['birthplace_name'], row['person_id'])

    context.db_cur.execute(query)

  context.db_conn.commit()

#================================================================================
# When

@when('I get samples born in Denmark')
def step_impl(context):
  context.prev_query = """
    SELECT *
      FROM civil.samples_born_in_denmark();
  """

  context.db_cur.execute(context.prev_query)

  context.prev_results = context.db_cur.fetchall()

  context.db_conn.commit()

@when('I check age of event of the previous results')
def step_impl(context):
  context.prev_query = """
    WITH failure AS (
      %s
    ) SELECT peo.id AS person_id
           , civil.age_at_event(peo.born_at, failure.failure_at)::int AS age
        FROM civil.people AS peo
             INNER JOIN failure
                     ON peo.id = failure.person_id
  """ % context.prev_query

  context.db_cur.execute(context.prev_query)

  context.prev_results = context.db_cur.fetchall()

  context.db_conn.commit()
