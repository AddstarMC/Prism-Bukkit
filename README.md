#Prism

Prism is a new generation of grief management (rollbacks, restores, etc) plugin for your Bukkit server. It's designed to be extremely fast and incredibly powerful - designed by a group with a lot of experience in both programming and owning/managing a very popular/high-traffic server.

## Features

- **Extremely fast. Rolls back thousands of broken blocks within milliseconds. No lag.**
- **Several easy, yet powerful lookup/inspector commands.**
- **Rollback, restoration commands like you know and love.**
- **Preview mode shows rollback AND restore actions to you only - then you can apply/cancel before actually changing the world.**
- **Tracks essentially everything you need to know. (Configurable, see below)**
- **Amazingly easy-to-use commands**
- **Safe defaults so you don't accidentally roll back too much with a typo**
- **Extremely clean, personal, and beautiful chat messages, that NEVER spam you.**
- **Configurable for your needs. Change the defaults, ignore players/worlds, ignore creative, disable tracking events you don't care about!**
- Tracks essentially ALL block changes.
- Includes related changes like blocks falling, signs/torches popping off
- Tracks sign text and even restores it upon rollbacks.
- Associates those related activities to the original block breaking player.
- Mushrooms and tree growth is associated with the planting player.
- Rolling back fire damage also puts out any remaining fires. (Configurable)
- Or just use the extinguish command.
- Rolling back tnt/creeper explosions also removes the laggy item drops within the radius. (Configurable)
- Drain water/lava with the drain command.
- Track and rollback animal/mob kills. Someone killed your cows? No problem.*
- Track shearing of sheep.
- Track mobs changing blocks like enderman, sheep eating, etc.
- Tracks chest/furnace/dispenser/etc access. 
- Watches for someone placing/taking items from the containers.
- You can even rollback items stolen from the container.*
- Records player deaths and what the cause was, who the attacker was.
- Records items player dropped, and items they pickup. Know who stole your death spot loot!
- Find the right info with paginated near/lookup commands.
- Get specific with a block history inspector bound to your hand.
- Database is designed specifically for the data we store. No disk space bloating (not going to point fingers...)



## How to Install
	
Place the Prism.jar file in your bukkit `/plugins` directory. Start your server and an initial configuration file will be created.

At the very least, you must set the MySQL connection information for your database before using Prism.
	

## How to Use

By default, Prism will begin tracking all sorts of events that happen in your worlds. Players making block changes, killing entities, transfers of items, and much more. 

Some of this data is purely informational, so you can find out who did something, and some of it is useful for rollbacks and restores.


#### Searching (Lookup, Inspector, Near)

The first use is to be able to find the information you need. Prism comes with three tools to find the data from in-game, each with a different focus.

##### Inspector

Use `/prism i` to toggle your hand/tool into inspection mode. Left-click a block to see that exact block's history. Right-click on any side of the block to see the history for the space adjacent.

Excellent for finding information about a single block, like a chest or ore finds.

##### Near

Use `/prism near` to do a quick query for all actions within a five block radius (configurable). It shows you all actions so you can quickly get a picture of what's happened.

Excellent for an area with no specific/visible issues. For example you can't tell what was griefed or you need to see if anyone was here recently.

##### Lookup

When the inspector and near commands aren't enough you have the power to query essentially any results from the database.

Use `/prism l (params)` to do a search of information. **See below for understanding parameters.** By using any arrangement of the parameters you can easily filter through records and find out exactly what you need.

Lookup queries can be local to you, or even global if you so desire. Need to see only all item-pickup actions by player nasonfish within ten blocks? Or how about where player YeaItsMe last placed a chest in the entire server.

It's almost limitless.

##### Pagination

Most times, a lookup or near query returns a lot of information. We break this into pages for easier navigating. 

After *any* lookup or near query, use `/prism page [page #]` to skip to different pages of results. It simply uses the data from your last search and will expire after five minutes.


##### Teleporting

When you perform a lookup with `r:global`, the results will include ID numbers with every action. Since you're viewing records that aren't nearby you can teleport to them.

Use `/prism tp [id]` to teleport to a record's location. 

*Note: We're working on a smarter teleport system that will accurately judge the safety of the area, and attempt a nearby spot. This is in development.*


#### Understanding Parameters

"Parameters" is the name we use for *arguments* given to commands like lookup, preview, rollback, and restore. They allow you to define exactly which data Prism should be working with.

You may define any, all, or none (safe defaults assumed if none given). You may use them in any order.

You can even define multiple arguments for most by separating them with a comma. Like `a:block-break,block-place`

**List of parameters:**

- `a:[action]` - Like "block-break" (See below for full list). No default.
- `r:[radius]` - How many blocks near you the action happened, i.e. `r:20`. Default radius set in config. Can't exceed the max defined in config. Use r:global to force an all-world search, *for lookups only*.
- `b:[blockname/id]` - Like `b:grass` or `b:2` or `b:2:0`. No default.
- `e:[entity]` - Like `e:pig`. No default.
- `t:[timesince]` - Events after x long ago. Like 1s(seconds), 20m(minutes), 1h(hour), 7d(days), 2w(weeks). No default. Use time arguments together if you wish, like `1h20m`.
- `p:[player]` - Like `p:viveleroi`. No default.
- `w:[world]` - Like `w:worldname`. Defaults to your current world.


**List of actions:**





#### Preview

*Previewing* a rollback or restore is **the safest way**. When you preview a rollback/restore, Prism queries the database with your given parameters, and shows those blocks changes/restorations to you only - it does not apply the rollback/restore to the world. Players around you won't see the preview, only you will.

This allows you to see whether or not your rollback/restore will do what you intended - because if not you can cancel it without any impact on the real map.

Use `/prism preview rollback (params)` (or restore). If the results look good and you wish to apply them to the map, use `/prism preview apply`.

If not, use `/prism preview cancel`. You may only have one preview pending at a time, and they will expire after a minute. If you forget about them or logout, they will self-cancel.

Applying a preview doesn't require the (params) again because Prism remembers what you had queried. When you apply a preview, it's done exactly the same way but as a real rollback/restore.

***The Prism team recommends you make a habit of always using preview.***

*Note: Only block changes are supported in preview mode. Item/entity/etc rollbacks won't show in a preview, but will still be rolled back correctly when the preview is applied.*

#### Rollback

Some expert users may be comfortable enough to rollback without a preview. Use `/prism rollback (params)`. Your changes will be applied immediately.

*Note: The Prism team reminds you that if you allow rollbacks, especially of items removed from chests or entity kills, be aware that staff with access to do so have the capability to do so repeatedly. There's no absolute way to ensure an item or entity hasn't already been rolled back but you shouldn't have a problem because only staff can perform rollbacks and staff are by definition trusted. However, you can disable item removal rollbacks in the config.*


#### Restore

Restoring is a way to re-apply changes that usually have been rolled back already. Essentially a method to reverse a rollback.

Use `/prism restore (params)`. 

*Note: Both rollbacks and restores of block-place actions will try their best to avoid re-apply block changes to an area with newer activity. For example if you re-apply a block-place I did, but Natman93 has already put a new block there, Prism will skip it.*


#### Ex, Drain

Use `/prism ex [radius]` to extinguish all fires in the radius, or `/prism drain [radius]` to remove all liquid (water and lava) in the radius.

When performing a rollback of block-burn actions, Prism will automatically extinguish the fires as well. 


#### Delete

Server operators can use `/prism delete [timeframe]` to manually purge records from the database. This isn't usually necessary, as Prism will automatically clear records that have expired (per config) on startup.


## Full Commands List

- `prism i`
- `prism l (params)`
- `prism near`
- `prism page [#]`
- `prism preview rollback (params)`
- `prism preview restore (params)`
- `prism preview apply`
- `prism preview cancel`
- `prism rollback (params)`
- `prism restore (params)`
- `prism ex [radius]`
- `prism drain [radius]`
- `prism tp [id]]`
- `prism delete`
- `prism ?` - Help.
- `prism params` - List parameters in-game.



## Permissions

- `prism.help`
- `prism.lookup` - Grants config lookup, inspector, near permission. Recommended for: Staff
- `prism.extinguish` - For removed fires in a radius.
- `prism.drain` - Removing water/lava in a radius.
- `prism.preview` - Grants permission to preview and then rollback/restore changes. Recommended for: Staff
- `prism.rollback` - Grants permission to rollback changes. Recommended for: Experienced Staff
- `prism.restore` - Grants permission to restore (reapply) changes. Recommended for: Experienced Staff       
- `prism.tp` - Teleport to a record ID
- `prism.reload` - Grants config reload permission. Recommended for: OPs
- `prism.delete` - Purge records from database via commands. Recommended for: OPs
- `prism.*` - Grants all permissions. Recommended for: OPs

## Known Issues

- Hanging items can't be rolled back because of a bukkit bug with their facing directions. https://bukkit.atlassian.net/browse/BUKKIT-3371

          
## Get Help

IRC: irc.esper.net #dhmc_us

Wiki: http://dhmc.us/wiki/view/prism/          
           
## Credits

This plugin was custom designed for the amazing *dhmc.us* Minecraft server.


## Authors

- viveleroi (Creator, Lead Developer)
- nasonfish (Contributor)
- nasonfish, Natman93, YeaItsMe (Dev Testers)
- @todo (Live Testers)