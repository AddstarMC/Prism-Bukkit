.. _mysql:

Mysql
=====

Installing in Ubuntu - honestly just run `sudo apt-get install mysql-server` follow the prompts done but you can find a comprehensive guid here: `MySQL Installation <https://www.digitalocean.com/community/tutorials/how-to-install-mysql-on-ubuntu-18-04>`_  You may need to use google to find your specific server type.

On Windows (and I reinterate Windows IS NOT A Operating System to run a Minecraft Server) but if you have to  `Mysql On Windows <https://www.mysqltutorial.org/install-mysql/>`_

Once installed configurating Prism is pretty straight forward.  Open a MYSQL command line and type

.. code:: sql

  CREATE SCHEMA `prism`;
  CREATE USER `prism`@`localhost` identified by `prism`;
  GRANT ALL PRIVILEGES ON prism.* TO 'prism'@'localhost';

Keep in mind if you server is not running on the same machine you will need to adjust the `localhost` string to the appropriate host name.  If you are not sure adjust it to `%`  but this has security implications.


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

You should adjust the properties "hostname", "username" and "password" to suit your configuration.  If you are using MySql leave "useNonStandardSql" as true for MariaDb see :ref:`mariadb` set it as false.
Percona is a fork of MYSQL that aims to be a performant version of mysql that is fully mysql compatible.
