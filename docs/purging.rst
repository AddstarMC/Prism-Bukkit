.. _purging:

#######
Purging
#######

Prism will automatically purge old data based on your rules defined in the config. This helps keep the database size down which impacts the speed of your lookups, rollbacks, etc.

You can configure purge rules by providing a list of parameters in prism.db-records-purge-rules just like you would do in-game.

How Purging Works
=================

Prism initializes the purge manager on server startup
Every 12 hours (not tick-based) the purge cycle runs asynchronously
Purge rules are handled one at a time
Prism will "chunk" the database queries (see below)
Once complete prism will list the total number of records removed and move on to the next rule

What is Chunking
================

Chunking refers to the practice of scanning a limited number of records by their primary key each cycle, and then looking for records that match your parameters. No matter the conditions, queries will be extremely fast and will only lock the exact number of entries scanned. This helps prevent lock exhaustion errors and keeps the locking out of the way of new inserts from your running server.

The config entry ``prism.purge.records-per-batch`` refers to how many records will be scanned each cycle - by their primary keys - to find matches for your conditions. There will very likely be cycles in which the purge system doesn't find any records.

The config entry ``prism.purge.batch-tick-delay`` refers to the time in ticks between each cycle. After each cycle scanning the records-per-batch, Prism will wait this time before starting the next cycle.

You should adjust the range records-per-batch and batch-tick-delay based on what your database server and size can manage. Prism defaults to 100,000 records per batch and a 30 tick (1.5 second) delay between batches. You can adjust them based on your needs.

Prism will report the maximum cycle time (time to scan records-per-batch) of each purge rule in milliseconds to help you adjust the purge parameters. In general, you want the largest number of records without the maximum cycle time becoming excessive. A good starting point might be to set records-per-batch such that the maximum cycle time is approximately equal to the batch-tick-delay.

Since each cycle is generally low-impact to the database because of chunking, it's very efficient to run while your server keeps writing new data.