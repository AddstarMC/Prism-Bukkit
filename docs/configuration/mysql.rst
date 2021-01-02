Mysql
=====

Mysql configuration looks typically like:-

.. code:: yaml

  datasource:
    type: mysql
    properties:
      hostname: 127.0.0.1
      username: prism
      password: prism
      databaseName: prism
      prefix: prism_
      port: '3306'
      useNonStandardSql: true

You should adjust the properties "hostname", "username" and "password" to suit your configuration.  If you are using MSYQL leave "useNonStandardSql" as true for MariaDb set it as false.
