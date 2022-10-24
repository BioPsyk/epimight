{% extends "with-relatives.sql" %}

{% block tte %}
  {% include "two-disorders-exclusion.sql" %}
{% endblock %}

{% block final_select %}
  {{ super() }}
{% endblock %}
