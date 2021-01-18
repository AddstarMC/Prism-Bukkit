****
Undo
****

**Undo is currently only available for drain events.**

Prism has an extremely unique undo system, all because it was designed to essentially work as rollback/restore system for Prism events.
Essentially, we record rollback, restore, drain, and other events just as we record player, entity, and world events.

The benefit of this process are:
- You can see a record of rollbacks, restores, drains, etc without command logging.
- You can easily undo the last action, which is most common.
- You can undo any supported action with its ID number.
- This means you don't lose the ability to undo when you crash / log off.
- Other staff may undo an action done by someone else.

Undo
====

After you use `/prism drain` on liquid, you can undo it.

Use `/prism undo last` to reverse the most recent drain action.

Use `/prism undo` to list all drain actions that have been recorded. Each record will show an ID and with the id, you can undo any drain event that shows.

`/prism undo [id]`