{% extends "with-relatives.sql" %}

{% block tte %}
  {% include "single-disorder.sql" %}
{% endblock %}

{% block final_select %}
  {{ super() }}
{% endblock %}
