from behave import *

#================================================================================
# Query generators

def determine_failure_query(context, study_end_at):
  phenos = context.array_from_table(
    context,
    ['phenotype_origin', 'phenotype_id'],
    'genetics.phenotype_id'
  )
  study_end_at = "'%s'::date" % study_end_at

  return """
    WITH diagnosed_people AS (
      SELECT peo.id AS person_id
           , peo.born_at
           , %s as study_end_at
           , dia.diagnosis_at
           , peo.status_changed
           , peo.status
        FROM civil.people AS peo
             LEFT JOIN medical.earliest_diagnoses_by_phenotypes(%s) AS dia
                     ON peo.id = dia.person_id
    ) SELECT person_id
           , failure_status
           , failure_at
           , failure_time
        FROM diagnosed_people
           , epidemiology.determine_failure( born_at
                                           , study_end_at
                                           , diagnosis_at
                                           , status_changed
                                           , status
                                           )
    ORDER BY person_id
  """ % (study_end_at, phenos)

#================================================================================
# When

@when('I determine failure using study end at "{study_end_at:ti}" and the following phenotypes')
def step_impl(context, study_end_at):
  context.prev_query = determine_failure_query(context, study_end_at)

  context.db_cur.execute(context.prev_query)

  context.prev_results = context.db_cur.fetchall()

  context.db_conn.commit()

@when('I sum failure of the previous results')
def step_impl(context):
  context.prev_query = """
    WITH failure AS (
      %s
    ) SELECT (epidemiology.sum_failure(failure_status, failure_time)).*
        FROM failure
  """ % context.prev_query

  context.db_cur.execute(context.prev_query)

  context.prev_results = context.db_cur.fetchall()

  context.db_conn.commit()
