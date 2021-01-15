What does Skipped mean?
=======================

- Entities that prism will refuse to rollback. Currently: Creepers
- Blocks that prism will refuse to rollback: fire, tnt, lava
- For some reason crops aren't rolling back onto soil (if not dirt/grass/air)
- For some reason lilypads aren't rolling back on water (water or air are ok)
- If the location that we're trying to place a block into has changed from what we expect. Example - if a player A places stone and breaks it, and player B places a gold block in that same spot, a rollback of `p:playerA a:break` will be skipped because Prism doesn't want to mess up what Player B has done. To force this, use the `-overwrite` flag during rollbacks.