Restore
=======

Restoring is a way to re-apply changes that usually have been rolled back already.
Essentially a method to reverse a rollback.

Use `/prism restore (params)`.

_Note: Both rollbacks and restores of block-place actions will try their best to avoid re-applying block changes to an area with newer activity. For example if you re-apply a block-place I did, but 'Natman93' has already put a new block there, Prism will skip it._