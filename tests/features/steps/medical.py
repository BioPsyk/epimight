from behave import *

#================================================================================
# Given

@given('person {person_id:d} got a "{kind:w}" ICD-{icd_edition:d} "{icd_id:w}" diagnosis at "{diagnosis_at:ti}"')
def step_impl(context, person_id, kind, icd_edition, icd_id, diagnosis_at):
  context.insert_diagnosis(context, person_id, kind, icd_edition, icd_id, diagnosis_at)

#================================================================================
# Given using sets

@given('person {person_id:d} was diagnosed with the following set of diagnoses')
def step_impl(context, person_id):
  for row in context.table:
    context.insert_diagnosis(
      context,
      person_id, row['kind'], row['icd_edition'], row['icd_id'], row['diagnosis_at']
    )

#================================================================================
# When using sets

@when('I get earliest diagnoses by the following set of phenotypes')
def step_impl(context):
  phenos = context.array_from_table(
    context,
    ['phenotype_origin', 'phenotype_id'],
    'genetics.phenotype_id'
  )

  context.prev_query = """
    SELECT person_id
         , phenotype_origin
         , phenotype_id
         , diagnosis_at
         , diagnosis_kind
      FROM medical.earliest_diagnoses_by_phenotypes(%s);
  """ % (phenos)

  context.db_cur.execute(context.prev_query)

  context.prev_results = context.db_cur.fetchall()

  context.db_conn.commit()
