==========================
Custom Database Connection
==========================
Sometimes you want an entirely custom db connection.  As long as your chosen connector supports most of the modern MySQL style SQL commands it should work.  You can use "Hikari" to configure this.
Adjust your configuration to show as follows

.. code-block:: yaml

  datasource:
    type: hikari
    properties:
      prefix: prism_
      useNonStandardSql: true

You will need to adjust the Hikari.properties file appropriately and provide the JDBC connector on the classpath.  Please see the `HikariCP project <https://github.com/brettwooldridge/HikariCP#configuration-knobs-baby>`_ for examples.  The table prefix and the use of non standard sql can still be chose here.

.. code-block:: properties

    #Prism Hikari Datasource Properties for advanced database Configuration
    #Sat Jan 02 21:47:38 AEST 2021
    initializationFailTimeout=10
    validationTimeout=5000
    readOnly=false
    registerMbeans=false
    isolateInternalQueries=false
    maxLifetime=1800000
    leakDetectionThreshold=0
    minimumIdle=1
    allowPoolSuspension=false
    idleTimeout=600000
    jdbcUrl=jdbc\:mysql\://localhost\:3306/prism?useUnicode\=true&characterEncoding\=UTF-8&useSSL\=false
    maximumPoolSize=4
    autoCommit=true
    connectionTimeout=30000
    poolName=prism
    username=prism
    password=prism

This is an example of hikari properties ....which replicates the mysql connection.