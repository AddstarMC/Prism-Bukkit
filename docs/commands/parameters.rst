##########
Parameters
##########

"Parameters" is the name we use for **arguments** given to commands like lookup, preview, rollback, and restore.
They allow you to define exactly which data Prism should be working with.

You may define any, all, or none (safe defaults assumed if none given).
You may use them in any order.

You can even define multiple arguments for most by separating them with a comma.
Like ``a:break,place``

A common complaint has been that players get the error:
``You're missing valid actions. Use /prism ? for assistance.``

Please apply the permissions node: ``prism.parameters.action.required`` - set this as negated - ie |:x:|. for the
player in question.

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

.. list-table:: Action List
  :widths: auto
  :header-rows: 1
  :align: center

  * - Action
    - Rollback
    - Restore
    - Description
    - Perm State
  * - block-break
    - |:heavy_check_mark:|
    - |:heavy_check_mark:|
    - Any broken block.
    - |:heavy_check_mark:|
  * - block-burn
    - |:heavy_check_mark:|
    - |:heavy_check_mark:|
    - Any burnt block.
    - |:heavy_check_mark:|
  * - block-dispense
    - |:x:|
    - |:x:|
    - Dispenses an item from a block
    - |:heavy_check_mark:|
  * - block-fade
    - |:heavy_check_mark:|
    - |:heavy_check_mark:|
    - | A block that fades ie snow melts.
      | Leaves fade if disconnected.
    - |:heavy_check_mark:|
  * - block-fall
    - |:heavy_check_mark:|
    - |:heavy_check_mark:|
    - ie Sand falling
    - |:heavy_check_mark:|
  * - block-form
    - |:heavy_check_mark:|
    - |:heavy_check_mark:|
    - Cobble Forming or Ice forming.
    - |:heavy_check_mark:|
  * - block-place
    - |:heavy_check_mark:|
    - |:heavy_check_mark:|
    - Any block placed.
    - |:heavy_check_mark:|
  * - block-shift
    - |:x:|
    - |:x:|
    - Blocks shifted by Pistons etc.
    - |:heavy_check_mark:|
  * - block-spread
    - |:heavy_check_mark:|
    - |:heavy_check_mark:|
    - Blocks spreading organically - ie grass
    - |:heavy_check_mark:|
  * - block-use
    - |:x:|
    - |:x:|
    - Using a block ie Crafting table.
    - |:heavy_check_mark:|
  * - bucket-fill
    - |:x:|
    - |:x:|
    - Filling a bucket.
    - |:heavy_check_mark:|
  * - bonemeal-use
    - |:x:|
    - |:x:|
    - Using bone meal.
    - |:heavy_check_mark:|
  * - container-access
    - |:x:|
    - |:x:|
    - Using a container.
    - |:heavy_check_mark:|
  * - cake-eat
    - |:x:|
    - |:x:|
    - Eating cake
    - |:heavy_check_mark:|
  * - craft-item
    - |:x:|
    - |:x:|
    - Crafting an item
    - |:x:|
  * - creeper-explode
    - |:heavy_check_mark:|
    - |:heavy_check_mark:|
    - Creeper exploding
    - |:heavy_check_mark:|
  * - crop-trample
    - |:heavy_check_mark:|
    - |:heavy_check_mark:|
    - Crop Damage
    - |:heavy_check_mark:|
  * - dragon-eat
    - |:heavy_check_mark:|
    - |:heavy_check_mark:|
    - Dragon eating blocks.
    - |:heavy_check_mark:|
  * - enchant-item
    - |:heavy_check_mark:|
    - |:heavy_check_mark:|
    - Creeper exploding
    - |:x:|
  * - enderman-pickup
    - |:heavy_check_mark:|
    - |:heavy_check_mark:|
    - Enderman picking up blocks.
    - |:heavy_check_mark:|
  * - enderman-place
    - |:heavy_check_mark:|
    - |:heavy_check_mark:|
    - Enderman placing a block.
    - |:heavy_check_mark:|
  * - entity-break
    - |:heavy_check_mark:|
    - |:heavy_check_mark:|
    - Entity breaks a block?
    - |:heavy_check_mark:|
  * - entity-dye
    - |:x:|
    - |:x:|
    - Dying something.
    - |:x:|
  * - entity-explode
    - |:heavy_check_mark:|
    - |:heavy_check_mark:|
    - As per creeper.
    - |:heavy_check_mark:|
  * - entity-follow
    - |:x:|
    - |:x:|
    - Entity follows a player.
    - |:heavy_check_mark:|
  * - entity-form
    - |:heavy_check_mark:|
    - |:heavy_check_mark:|
    - Entity created.
    - |:heavy_check_mark:|
  * - entity-kill
    - |:heavy_check_mark:|
    - |:heavy_check_mark:|
    - Entity killed.
    - |:heavy_check_mark:|
  * - entity-leash
    - |:x:|
    - |:x:|
    - Entity is leashed.
    - |:heavy_check_mark:|
  * - entity-shear
    - |:x:|
    - |:x:|
    - Entity is shorn
    - |:heavy_check_mark:|
  * - entity-spawn
    - |:x:|
    - |:x:|
    - Entity is spawned.
    - |:heavy_check_mark:|
  * - entity-unleash
    - |:x:|
    - |:x:|
    - Entity is unleashed,.
    - |:heavy_check_mark:|
  * - fireball
    - |:x:|
    - |:x:|
    - Firball ignited.
    - |:heavy_check_mark:|
  * - fire-spread
    - |:heavy_check_mark:|
    - |:heavy_check_mark:|
    - Fire spreading
    - |:heavy_check_mark:|
  * - firework-launch
    - |:heavy_check_mark:|
    - |:heavy_check_mark:|
    - Firework launching
    - |:heavy_check_mark:|
  * - hangingitem-break
    - |:heavy_check_mark:|
    - |:heavy_check_mark:|
    - Painting etc Broken
    - |:heavy_check_mark:|
  * - hangingitem-place
    - |:heavy_check_mark:|
    - |:heavy_check_mark:|
    - Painting etc placed.
    - |:heavy_check_mark:|
  * - item-drop
    - |:heavy_check_mark:|
    - |:heavy_check_mark:|
    - Items dropped.
    - |:heavy_check_mark:|
  * - item-insert
    - |:heavy_check_mark:|
    - |:heavy_check_mark:|
    - Item inserted into chest etc.
    - |:heavy_check_mark:|
  * - item-pickup
    - |:heavy_check_mark:|
    - |:heavy_check_mark:|
    - Item picked up off the ground.
    - |:heavy_check_mark:|
  * - item-remove
    - |:heavy_check_mark:|
    - |:heavy_check_mark:|
    - Painting etc picked up
    - |:heavy_check_mark:|
  * - item-rotate
    - |:x:|
    - |:x:|
    - Item frame rotated.
    - |:heavy_check_mark:|
  * - lava-break
    - |:x:|
    - |:x:|
    - Lava broke a block
    - |:heavy_check_mark:|
  * - lava-bucket
    - |:heavy_check_mark:|
    - |:heavy_check_mark:|
    - Lava collected
    - |:heavy_check_mark:|
  * - lava-flow
    - |:heavy_check_mark:|
    - |:heavy_check_mark:|
    - Lava flows.
    - |:heavy_check_mark:|
  * - lava-ignite
    - |:x:|
    - |:x:|
    - lava ignites surrounding squares
    - |:heavy_check_mark:|
  * - leaf-decay
    - |:heavy_check_mark:|
    - |:heavy_check_mark:|
    - Painting etc placed.
    - |:heavy_check_mark:|
  * - lighter
    - |:x:|
    - |:x:|
    - Using a lighter
    - |:heavy_check_mark:|
  * - lightning
    - |:x:|
    - |:x:|
    - Lightning strike
    - |:heavy_check_mark:|
  * - mushroom-grow
    - |:heavy_check_mark:|
    - |:heavy_check_mark:|
    - Mushroom grows
    - |:heavy_check_mark:|
  * - player-chat
    - |:x:|
    - |:x:|
    - Chat
    - |:x:|
  * - player-command
    - |:x:|
    - |:x:|
    - A player command
    - |:x:|
  * - player-death
    - |:x:|
    - |:x:|
    - A player death
    - |:heavy_check_mark:|
  * - player-join
    - |:x:|
    - |:x:|
    - Joining a server
    - |:x:|
  * - player-kill
    - |:heavy_check_mark:|
    - |:x:|
    - A player kill
    - |:x:|
  * - player-quit
    - |:x:|
    - |:x:|
    - Leaving a server
    - |:x:|
  * - player-teleport
    - |:x:|
    - |:x:|
    - Teleporting
    - |:x:|
  * - potion-splash
    - |:x:|
    - |:x:|
    - A potion splash occurs
    - |:heavy_check_mark:|
  * - sheep-eat
    - |:x:|
    - |:x:|
    - Sheep eating grass
    - |:heavy_check_mark:|
  * - sign-
    - |:x:|
    - |:heavy_check_mark:|
    - Sign is placed
    - |:heavy_check_mark:|
  * - spawnegg-use
    - |:x:|
    - |:x:|
    - Using a spawn egg
    - |:heavy_check_mark:|
  * - tnt-explode
    - |:heavy_check_mark:|
    - |:heavy_check_mark:|
    - Using TNT
    - |:heavy_check_mark:|
  * - tnt-prime
    - |:x:|
    - |:x:|
    - Priming TNT
    - |:x:|
  * - tree-grow
    - |:heavy_check_mark:|
    - |:heavy_check_mark:|
    - Tree grows
    - |:heavy_check_mark:|
  * - vehicle-break
    - |:heavy_check_mark:|
    - |:x:|
    - Vehicle Breaks
    - |:heavy_check_mark:|
  * - vehicle-enter
    - |:x:|
    - |:x:|
    - Vehicle is entered
    - |:heavy_check_mark:|
  * - vehicle-exit
    - |:x:|
    - |:x:|
    - Vehicle is exit
    - |:heavy_check_mark:|
  * - vehicle-place
    - |:x:|
    - |:x:|
    - Vehicle Placed
    - |:heavy_check_mark:|
  * - water-break
    - |:heavy_check_mark:|
    - |:heavy_check_mark:|
    - Water Breaks blocks
    - |:heavy_check_mark:|
  * - water-bucket
    - |:heavy_check_mark:|
    - |:heavy_check_mark:|
    - Water Bucket Used
    - |:heavy_check_mark:|
  * - water-flow
    - |:heavy_check_mark:|
    - |:heavy_check_mark:|
    - Water flowing
    - |:x:|
  * - world-edit
    - |:heavy_check_mark:|
    - |:heavy_check_mark:|
    - Make a world edit
    - |:x:|
  * - xp-pickup
    - |:x:|
    - |:x:|
    - Xp Collected
    - |:x:|
  * - target-hit
    - |:x:|
    - |:x:|
    - A Target block struck by Arrow
    - |:x:|
  * - player-trade
    - |:x:|
    - |:x:|
    - Player trades with villager
    - |:x:|
  * - item-receive
    - |:heavy_check_mark:|
    - |:heavy_check_mark:|
    - An item is received.
    - |:x:|

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
