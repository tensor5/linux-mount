Version 0.2.0.0
---------------

- New functions has been introduced to perform specific mount actions,
  previously performed with a combination of `mount` and the appropriate
  `MountFlag`. Consequently the corresponding `MountFlag`'s have been
  removed. Below is the list of new functions.
    * Remount a filesystem:
        - remount
    * Bind a filesystem:
        - bind
        - rBind
        - rebind
    * Change propagation flags:
        - makeShared
        - makeRShared
        - makeSlave
        - makeRSlave
        - makePrivate
        - makeRPrivate
        - makeUnbindable
        - makeRUnbindable
    * Move a filesystem to a different mount point:
        - move
- Rename some `MountFlag`'s, and remove those only valid in kernel-space.
- Replace `Bool` with new `SymLink` type in function `umountWith`.
- Improve documentation.
- Minor code fixes.

Version 0.1.0.2
---------------

- Relicense under BSD3.
- Clean up code.
