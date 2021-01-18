#####
Flags
#####

.. tip::

  ``/pr flags``

This commands shows all the available flags.  There are some command flags that control specific details about the process you're running, giving you a whole new level of control over Prism.

Drain/Drain Water/Drain Lava
^^^^^^^^^^^^^^^^^^^^^^^^^^^^
``/prism (cmd) (params) -drain``

Adding a drain flag initiates a drain action (exactly as done by `/prism drain`) during the rollback. Use ``-drain-lava`` or ``-drain-water`` to target a specific liquid.

Extended
^^^^^^^^

Unless you've enabled extended logs in the config, we don't show you the extra info to reduce clutter.
But you may want it.
Use ``-extended`` to see the extended logs.

No Group
^^^^^^^^

Don't want to see actions grouped together?
Use ``-no-group``.

No Extinguish
^^^^^^^^^^^^^

``/prism (cmd) (params) -no-ext``

If configured, prism will automatically put out a burning fire when doing an `a:burn rollback`. If you need to disable this feature for a specific rollback, use this flag at command time.

Overwrite
^^^^^^^^^

``/prism (cmd) (params) -overwrite``

Overwrite any block that may happen to be in the way and would normally be skipped.

Per-Page
^^^^^^^^

``/prism (cmd) (params) -per-page=#``

The number of results for the current page.

Share
^^^^^

``/prism (cmd) (params) -share=#``

A list of online players to share the current lookup results with.

Paste
^^^^^

``/prism (cmd) (params) -paste``

Paste the result to "https://paste.gg" a paste service.