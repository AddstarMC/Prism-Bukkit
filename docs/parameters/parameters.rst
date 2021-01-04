##########
Parameters
##########

"Parameters" is the name we use for **arguments** given to commands like lookup, preview, rollback, and restore.
They allow you to define exactly which data Prism should be working with.

You may define any, all, or none (safe defaults assumed if none given).
You may use them in any order.

You can even define multiple arguments for most by separating them with a comma.
Like ``a:break,place``

Parameter List
==============

- ``a:[action]`` - Like "block-break" (See below for full list). Default is now all.
- ``r:[radius]`` - How many blocks near you the action happened, i.e. ``r:20``. Default radius set in config.
- ``r:global`` - Instructs Prism to not limit results to a location. Requires special configuration or permissions to be used for lookups/rollbacks/restores.
- ``r:we`` - Uses a WorldEdit selection to limit all res5ults - lookups/restores/rollbacks. Works for anything that supports the ``r`` parameter. Player must have WorldEdit selection permissions.
- ``r:player:[radius]`` Base a radius off of another online player.
- ``r:x,y,z:[radius]`` - Allow a radius around coordinates x,y,z
- ``r:world`` - No radius, but limited to the current world or the world specificed by the `w:` parameter.
- ``r:c`` - Limits the radius to the current chunk (x/z of the current chunk, from bedrock to world height)
- ``b:[blockname/id]`` - Like `b:grass` or `b:2` or `b:2:0`. No default.
- ``e:[entity]`` - Like `e:pig`. No default.
- ``t:[timesince]`` - Events after x long ago. Like 1s(seconds), 20m(minutes), 1h(hour), 7d(days), 2w(weeks). No default. Use time arguments together if you wish, like ``1h20m``.
- ``before:[time]`` - Events before x long ago.
- ``since:[time]`` - Events after x long ago. Same as t:
- ``p:[player]`` - Like ``p:viveleroi``. No default.
- ``w:[world]`` - Like `w:worldname`. Defaults to your current world.
- ``k:[world]`` - Text-based keyword search (for commands/chat mainly)
- ``id:[world]`` - A single record id.

Use ``!`` to exclude an action, player, or entity. For example: ``/pr rollback a:break p:!viveleroi``

.. _action-list:

Actions List
============

Short actions are simply the words after the hyphen. For example ``break`` would apply to everything that ends in ``*-break``

- block-break
- block-burn
- block-dispense
- block-fade
- block-fall
- block-form
- block-place
- block-shift
- block-spread
- block-use
- bucket-fill
- bonemeal-use
- container-access
- cake-eat
- craft-item
- creeper-explode
- crop-trample
- dragon-eat
- enchant-item
- enderman-pickup
- enderman-place
- entity-break
- entity-dye
- entity-explode
- entity-follow
- entity-form
- entity-kill
- entity-leash
- entity-shear
- entity-spawn
- entity-unleash
- fireball
- fire-spread
- firework-launch
- hangingitem-break
- hangingitem-place
- item-drop
- item-insert
- item-pickup
- item-remove
- item-rotate
- lava-break
- lava-bucket
- lava-flow
- lava-ignite
- leaf-decay
- lighter
- lightning
- mushroom-grow
- player-chat
- player-command
- player-death
- player-join
- player-kill
- player-quit
- player-teleport
- potion-splash
- sheep-eat
- sign-change
- spawnegg-use
- tnt-explode
- tnt-prime
- tree-grow
- vehicle-break
- vehicle-enter
- vehicle-exit
- vehicle-place
- water-break
- water-bucket
- water-flow
- world-edit
- xp-pickup

Understanding Action Relationships
==================================

Prism classifies different actions that are similar as different actions so that you have amazing power to find, rollback, and restore what you need.

Prism uses relationships in two ways.

Families
========
Action families simply refer to the fact that `creeper-explode` and `tnt-explode` are pretty much the same thing, just with two different causes.
If you use the short name `explode` it will find actions that match either of those sub-actions.
If you're specific, you'll only see results for one type.

Compare the normal action names with the short name list and you'll see a bunch more. `block-break` and `water-break` are nearly the same but can be tracked separately, but both can be queried with `a:break` if you wish.

Causality
=========
Some events are related because one causes another. In order to master rolling back griefs you really need to understand these.

Here's a simple example.
A wooden wall with a torch is burning down.
The block burns are recorded as `block-burn` but when the block holding the torch is removed the torch detaches, creating a `block-break` event.
Prism is very clear about what events mean so we don't want to record the torch as a burn event, because they can't burn.

Any near/lookup/inspect query will clearly show the events for you.

If you wish to roll back the entire wall, you should use `/prism rollback a:burn,break`.
Or if you need to very specific, `/prism rollback a:block-burn,block-break`.

Prism is smart enough to rollback the wall first and then re-attach the torch.

As you learn what the different events represent, you'll become a master with related actions.

Here are some more quick (detailed for your information) examples:
- `/prism rollback a:water-flow,water-break` - Water flow and any items it broke.
- `/prism rollback a:block-break,block-fade` - Tree broken and the leaves fading.

Command-Time Flags
==================
There are some command flags that control specific details about the process you're running, giving you a whole new level of control over Prism.

Drain/Drain Water/Drain Lava
----------------------------

``/prism (cmd) (params) -drain``

Adding a drain flag initiates a drain action (exactly as done by `/prism drain`) during the rollback. Use `-drain-lava` or `-drain-water` to target a specific liquid.

Extended
--------

``/prism (cmd) (params) -extended``

Unless you've enabled extended logs in the config, we don't show you the extra info to reduce clutter.
But you may want it.
Use `-extended` to see the extended logs.

No Group
---------

``/prism (cmd) (params) -no-group``

Don't want to see actions grouped together?
Use `-no-group`.

No Extinguish
-----------------
``/prism (cmd) (params) -no-ext``

If configured, prism will automatically put out a burning fire when doing an `a:burn rollback`. If you need to disable this feature for a specific rollback, use this flag at command time.

Overwrite
---------

``/prism (cmd) (params) -overwrite``

Overwrite any block that may happen to be in the way and would normally be skipped.


Per-Page
--------

``/prism (cmd) (params) -per-page=#``

The number of results for the current page.

Share
-----
``/prism (cmd) (params) -share=#``

A list of online players to share the current lookup results with.

Paste
-----

``/pr (cmd) (params) --paste``

Paste the result to a paste.gg site and show you the link.  Requires configuration.
