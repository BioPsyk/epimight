from behave import *
import psycopg2
import psycopg2.extras
import json

#================================================================================
# Common functionality

def clean_db(context):
  context.db_cur.execute("TRUNCATE civil.people      RESTART IDENTITY CASCADE")
  context.db_cur.execute("TRUNCATE medical.records   RESTART IDENTITY CASCADE")
  context.db_cur.execute("TRUNCATE medical.diagnoses RESTART IDENTITY CASCADE")

  context.db_conn.commit()

def medical_records_next_id(context):
  context.db_cur.execute(
    """
    SELECT id
      FROM medical.records
     ORDER BY id DESC
     LIMIT 1
    """
  )

  result = context.db_cur.fetchone()

  context.db_conn.commit()

  if result is None:
    return 1
  else:
    return result["id"] + 1

def insert_diagnosis(context, person_id, kind, icd_edition, icd_id, diagnosis_at):
  record_id = context.medical_records_next_id(context)

  context.db_cur.execute("""
    INSERT INTO medical.records
                (origin, id, person_id, started_at)
         VALUES ('pcrr', %s, %s, %s)
      RETURNING id
  """, (record_id, person_id, diagnosis_at))

  context.db_conn.commit()

  context.db_cur.execute("""
    INSERT INTO medical.diagnoses
                (record_origin, record_id, origin_id, kind, icd_edition, icd_id)
         VALUES ('pcrr', %s, %s, %s, %s, %s)
  """, (record_id, icd_id, kind, icd_edition, icd_id))

  context.db_conn.commit()

def is_numeric(inp):
    return all(char.isdigit() for char in inp)

def db_serialize(inp):
  result = []

  for val in inp:
    if val is None or val == '' or val == 'NULL':
      result.append('NULL')
    elif is_numeric(val):
      result.append(val)
    else:
      result.append("'%s'" % val)

  return result

def array_from_table(context, columns, array_type):
  result = []

  for row in context.table:
    values = []

    for col in columns:
      values.append(row[col])

    result.append(
      'ROW(%s)' % ', '.join(db_serialize(values))
    )

  return 'array[%s]::%s[]' % (','.join(result), array_type)

#================================================================================
# Hooks

def before_all(context):
  context.db_conn = psycopg2.connect(
    host     = 'localhost',
    dbname   = 'ibp_registry',
    user     = 'postgres',
    password = 'devpass'
  )

  context.db_cur = context.db_conn.cursor(
    cursor_factory=psycopg2.extras.RealDictCursor
  )

  clean_db(context)

  context.medical_records_next_id = medical_records_next_id
  context.insert_diagnosis        = insert_diagnosis
  context.array_from_table        = array_from_table
  context.db_serialize            = db_serialize

def after_all(context):
  context.db_cur.close()
  context.db_conn.close()

def before_scenario(context, scenario):
  clean_db(context)
