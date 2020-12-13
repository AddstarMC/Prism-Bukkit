Advanced Configuration
======================
We get technical here. If you're not sure what these recommendations refer to, please skip.

Pool Exhaustion
---------------
**Constantly seeing pool exhaustion errors?**

The cause normally isn't prism, but a problem communicating with the database fast enough.
There's a lot you can do.  If you have a remote mysql server, it's usually a lot slower because the server has to communicate across the internet every query, and shared mysql hosts are usually a bit lax on performance.

Recommendation:
^^^^^^^^^^^^^^^^
- Decrease the pool wait times.
- Increase the pool connection limit if your remote mysql provider as a limit that will fit it.
- If you can spare a little more memory, increase the actions per batch so a mysql query sends more data during larger changes.

Advanced Database Configurations
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Prism uses Hikari database pooling - please see Hikari for configuration options - changes can be made in  `Hikari.properties <https://github.com/brettwooldridge/HikariCP/wiki/>`.

Faster Event Logging
^^^^^^^^^^^^^^^^^^^^
Our defaults for the speed of event logging are set to sensible defaults with the medium-range servers average owners use. However, if you have more control over your server you can tweak Prism to record events faster.

- **actions-per-insert-batch** - refers to how many actions are sent per batch insert. The only things that limit this are memory (ram), and your mysql server's setting for max_allowed_packets. It's very possible to increase this number to 5000 or higher.  Every time the recorder runs, it will empty the queue with batch insert statements, and by changing it to 5000 instead of 1000, increases the speed that the queue is emptied dramatically. A 110k block world edit saves in 19 seconds with the default settings, but saves in 5 seconds with the increased batch size.

- **queue-empty-tick-delay** - This determines how many ticks (20 ticks = 1 second) the recorder will wait before checking the queue again. Setting this to a lower number will increase the speed of queue saves. For example, the default is 3 ticks which roughly means 6 queue empty actions per second.  When the recorder checks the queue, it will empty the entire queue in batches, so either way the queue will be emptied, this setting simply makes it check for newer actions more often.