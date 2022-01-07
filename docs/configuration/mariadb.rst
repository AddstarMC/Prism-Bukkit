.. _mariadb:

MariaDB
=======

MariaDB is not fully MYSQL compatible.  Thus it does not support all the functions available to a MYSQL server.  Thus we
only a subset of functions can be used.  Prism can support this by setting our NonStandardSQL parameter to false.  We still use the MYSQL driver as it is packed with most servers.  Alternatively you could use the advanced Hikari (see :ref:`hikari` configuration

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
      useNonStandardSql: false

