Remember: Join our Discord channel if you need more help.

How to Install
===============

- Place the Prism.jar file in your bukkit /plugins directory.
- Start your server and an initial configuration file will be created.
- At the very least, you must setup a database connection
- read below if you need help with MySQL, or sqlite (no-install database).

Get Mysql
---------
Make sure you have MySQL installed. Most of you likely do already, but some of you are new to MySQL.
MySQL is software that you can install on your computer (works on Windows, Mac, and Linux) that
creates a database service - a very fast, efficient place for Prism (and many other plugins) to store data.
Instructions for installed mysql.

1. Mysql - https://dev.mysql.com/doc/mysql-installation-excerpt/5.7/en/ see :ref:`mysql`

2. Mariadb - https://mariadb.com/kb/en/getting-installing-and-upgrading-mariadb/ see see :ref:`mariadb`

3. Percona  - https://www.percona.com/doc/percona-server/5.7/installation.html  see :ref:`mysql`

Step 1
------
Make sure the database.mode config says "mysql". Every MySQL connection needs to have an address, a username, a password, and a database.

Step 2
------
Enter your database hostname (the address to the mysql server, usually the default is 127.0.0.1 or
localhost. Enter the username/password for the server, defaults to root and an empty password.

*If you setup your MySQL server yourself, you should know what the above information is. If you're
using a mysql server setup by someone else (like on a shared hosting service), they will provide
you the information.*

Please see the relevant configuration section for your database type:

1. Mysql see :ref:`mysql`
2. MariaDB see see :ref:`mariadb`
3. Other Databases: see :ref:`hikari`

Recommendations
---------------
If you're concerned about disk space or if you have a busy server, we recommend leaving off water-flow and turning off lava-flow tracking. These events not only occur with extreme frequency,   but Bukkit also fires these events multiple times per block location. They can very quickly   saturate your database. It's relatively easy to use /prism drain instead.  However, Prism tracks lava/water-break events even if flow is disabled, so you can still rollback the items broken by the liquid.

