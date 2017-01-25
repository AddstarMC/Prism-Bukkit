**GPrism** is a Bukkit plugin that tracks changes to a Minecraft world. These changes can be looked
up, rolled back, restored, etc. There are also various anti-griefing tools.

GPrism is a downstream fork of @viveleroi's [Prism-Bukkit][PRISM], with various fixes and feature
additions by a few GitHub contributors and Roy Curtis.

For information on how to use GPrism, [see the original Prism wiki.][WIKI]

# Differences from upstream

## Pulled in from other forks

* 1.9, 1.9.4 and 1.10 support by @Jikoo
* UTF-8 MySQL support by @paulmory
* Inspect hand item and wand fixes by @Siggi88 and @Jikoo
* Removal of NMS usage and chat component API fixes by @Jikoo
* Item frame rollback fixes by @PhanaticD

## By Roy Curtis

* Removed duplicate GSON dependency and included Elixr (repository down)
* Added `useSSL=false` to MySQL string (due to spam from new versions)
* Hid purge cycle messages if 0 rows affected (reduces noise)
* More fixes to item frame tracking and rollbacks
* Made `item-rotate` action rollbackable and more accurate
* Fixed rollbacks of torches against walls
* Updated entity events and rollbacks to handle all the new 1.11 horse subtypes (incl. llamas)
* Fixed attribute rollbacks of killed horses
* Added wait messages to `/pr i` and `/pr near`
* Added support for rabbit types
* Attributed lingering potion deaths to players
* Java 1.8 language level
* POM file cleanup and improvements

# Support

Prism-Bukkit was abandoned in favor of the [Sponge rewrite][SPONGE]. This fork is not intended to be
a long-term continuation of Prism; this is only for our fixes, to keep Prism-Bukkit working. We may
remove features that we don't use or think should be provided by GPrism.

That said; please feel free to cherry pick commits or fork from this fork, or contribute to this
fork with issues and pull requests.

# Building, debugging and debug logging

For instructions and screenshots on how to. . .

* Compile this plugin from scratch
* Build a JAR of this plugin
* Debug this plugin on a server
* Enable debug logging levels such as `FINE` and `FINER`

. . .[please follow the linked guide on this Google document.][BUILD] However, for debugging, [you
*must* configure your IDE to extract (or "shade") these dependencies in any artifacts:][SHADE]

* `org.apache.tomcat:tomcat-jdbc:7.0.52`
* `org.apache.tomcat:tomcat-juli:7.0.52`
* `mkremins:fanciful:0.3.3-SNAPSHOT`

## License

As GPrism is a fork of Prism by viveleroi, GPrism is licensed the under the Creative Commons
Attribution-NonCommercial-ShareAlike 3.0 Unported. Please see `LICENSE` or [this website][LICENSE]
for the full license.

[PRISM]: https://github.com/prism/Prism-Bukkit
[SPONGE]: https://github.com/prism/Prism
[WIKI]: http://discover-prism.com/wiki/
[BUILD]: https://docs.google.com/document/d/1TTDXG7IZ9M0D2-rzbILAWg1CKjynHK8fNGxbf3W4wBk/view
[SHADE]: http://i.imgur.com/9eqRiwR.png
[LICENSE]: http://creativecommons.org/licenses/by-nc-sa/3.0/us/