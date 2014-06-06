--------------------------------------------------------------------------------
-- |
-- Module      :  $Header$
-- Copyright   :  © 2013-2014 Nicola Squartini
-- License     :  BSD3
--
-- Maintainer  :  Nicola Squartini <tensor5@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides an interface to the system mount and umount
-- functions. All functions below may fail with
-- @'System.IO.Error.isPermissionError'@ if the user does not have the required
-- privileges.
--
--------------------------------------------------------------------------------

module System.Linux.Mount
    ( -- * Mount a filesystem
      mount
    , remount

    -- ** Mount flags
    , MountFlag(..)
    , DriverData
    , noData

    -- * Bind a filesystem
    , bind, rBind
    , rebind

    -- * Change propagation flags

    -- | These functions change the propagation flag of an already mounted
    -- filesystem, as explained in
    -- <https://www.kernel.org/doc/Documentation/filesystems/sharedsubtree.txt>.
    -- They all take the mount point as argument.
    , makeShared, makeRShared
    , makeSlave, makeRSlave
    , makePrivate, makeRPrivate
    , makeUnbindable, makeRUnbindable

    -- * Move a filesystem
    , move

    -- * Unmount a filesystem
    , umount
    , umountWith

    -- ** Unmount flags
    , UmountFlag(..)
    , SymLink(..)

    ) where

#include <sys/mount.h>

import           Data.Bits       ((.|.))
import           Data.ByteString (ByteString, empty, useAsCString)
import qualified Data.ByteString as B
import           Foreign.C       (CInt (..), CString, CUInt, CULong (..),
                                  throwErrnoIfMinus1_, withCString)
import           Foreign.Ptr     (Ptr, castPtr, nullPtr)

-- | Mount a filesystem (call to
-- @<http://man7.org/linux/man-pages/man2/mount.2.html mount()>@).
mount :: String      -- ^ Device file
      -> FilePath    -- ^ Mount point
      -> String      -- ^ Filesystem type
      -> [MountFlag] -- ^ List of mount options
      -> DriverData  -- ^ Driver specific options
      -> IO ()
mount dev dir typ xs byt =
    throwErrnoIfMinus1_ "mount" $
    withCStringOrNull dev $ \cdev ->
        withCString dir $ \cdir ->
            withCString typ $ \ctyp ->
                useAsCStringOrNull byt $ \cdat->
                                   c_mount cdev
                                           cdir
                                           ctyp
                                           (combineBitMasks xs)
                                           (castPtr cdat)

withCStringOrNull :: String -> (CString -> IO a) -> IO a
withCStringOrNull []  f = f nullPtr
withCStringOrNull str f = withCString str f

useAsCStringOrNull :: ByteString -> (CString -> IO a) -> IO a
useAsCStringOrNull str f | B.null str = f nullPtr
useAsCStringOrNull str f              = useAsCString str f

-- | Alter flags of a mounted filesystem (call to
-- @<http://man7.org/linux/man-pages/man2/mount.2.html mount()>@ with
-- @MS_REMOUNT@).
remount :: FilePath    -- ^ Mount point
        -> [MountFlag] -- ^ List of mount options
        -> DriverData  -- ^ Driver specific options
        -> IO ()
remount dir xs byt =
    throwErrnoIfMinus1_ "mount" $
    withCString dir $ \cdir ->
        useAsCStringOrNull byt $ \cdat->
                           c_mount nullPtr
                                   cdir
                                   nullPtr
                                   (combineBitMasks xs .|. #{const MS_REMOUNT})
                                   (castPtr cdat)

-- | Mount an already mounted filesystem under a new directory (call to
-- @<http://man7.org/linux/man-pages/man2/mount.2.html mount()>@ with
-- @MS_BIND@).
bind :: FilePath  -- ^ Source mount point
     -> FilePath  -- ^ Target mount point
     -> IO ()
bind = mountSrcDest #{const MS_BIND}

-- | Mount an already mounted filesystem and all its submounts under a new
-- directory (call to
-- @<http://man7.org/linux/man-pages/man2/mount.2.html mount()>@ with @MS_BIND@
-- and @MS_REC@).
rBind :: FilePath  -- ^ Source mount point
      -> FilePath  -- ^ Target mount point
      -> IO ()
rBind = mountSrcDest (#{const MS_BIND} .|. #{const MS_REC})

-- | Atomically move a mounted filesystem to another mount point (call to
-- @<http://man7.org/linux/man-pages/man2/mount.2.html mount()>@ with
-- @MS_MOVE@).
move :: FilePath  -- ^ Old mount point
     -> FilePath  -- ^ New mount point
     -> IO ()
move = mountSrcDest #{const MS_MOVE}

mountSrcDest :: CUInt -> FilePath -> FilePath -> IO ()
mountSrcDest flag src dest =
    throwErrnoIfMinus1_ "mount" $
    withCString src $ \csrc ->
        withCString dest $ \cdest ->
            c_mount csrc cdest nullPtr (fromIntegral flag) nullPtr

-- | Alter flags of a bound filesystem (call to
-- @<http://man7.org/linux/man-pages/man2/mount.2.html mount()>@ with
-- @MS_REMOUNT@ and @MS_BIND@).
rebind :: FilePath     -- ^ Mount point
       -> [MountFlag]  -- ^ List of mount options
       -> IO ()
rebind dir flags =
    make (#{const MS_REMOUNT} .|. #{const MS_BIND}
          .|. fromIntegral (combineBitMasks flags)) dir

-- | Set the @MS_SHARED@ propagation flag on a mounted filesystem.
makeShared :: FilePath -> IO ()
makeShared = make #{const MS_SHARED}

-- | Set the @MS_SHARED@ propagation flag on a mounted filesystem and
-- recursively on all submounts.
makeRShared :: FilePath -> IO ()
makeRShared = make (#{const MS_SHARED} .|. #{const MS_REC})

-- | Set the @MS_SLAVE@ propagation flag on a mounted filesystem.
makeSlave :: FilePath -> IO ()
makeSlave = make #{const MS_SLAVE}

-- | Set the @MS_SLAVE@ propagation flag on a mounted filesystem recursively on
-- all submounts.
makeRSlave :: FilePath -> IO ()
makeRSlave = make (#{const MS_SLAVE} .|. #{const MS_REC})

-- | Set the @MS_PRIVATE@ propagation flag on a mounted filesystem.
makePrivate :: FilePath -> IO ()
makePrivate = make #{const MS_PRIVATE}

-- | Set the @MS_PRIVATE@ propagation flag on a mounted filesystem and
-- recursively on all submounts.
makeRPrivate :: FilePath -> IO ()
makeRPrivate = make (#{const MS_PRIVATE} .|. #{const MS_REC})

-- | Set the @MS_UNBINDABLE@ propagation flag on a mounted filesystem.
makeUnbindable :: FilePath -> IO ()
makeUnbindable = make #{const MS_UNBINDABLE}

-- | Set the @MS_UNBINDABLE@ propagation flag on a mounted filesystem and
-- recursively on all submounts.
makeRUnbindable :: FilePath -> IO ()
makeRUnbindable = make (#{const MS_UNBINDABLE} .|. #{const MS_REC})

make :: CUInt -> FilePath -> IO ()
make flag dir =
    throwErrnoIfMinus1_ "mount" $
    withCString dir $ \cdir ->
        c_mount nullPtr cdir nullPtr (fromIntegral flag) nullPtr

foreign import ccall unsafe "mount"
  c_mount :: CString -> CString -> CString -> CULong -> Ptr a -> IO CInt

-- | Unmount a filesystem (call to
-- @<http://man7.org/linux/man-pages/man2/umount.2.html umount()>@).
umount :: FilePath -- ^ Mount point
       -> IO ()
umount str = throwErrnoIfMinus1_ "umount" (withCString str c_umount)

foreign import ccall unsafe "umount"
  c_umount :: CString -> IO CInt

-- | Unmount a filesystem using specific unmount options (call to
-- @<http://man7.org/linux/man-pages/man2/umount.2.html umount2()>@).  See
-- @'UmountFlag'@ for details.
umountWith :: UmountFlag -- ^ Unmount option
           -> SymLink    -- ^ @'Follow'@ or @'NoFollow'@ symbolic links
           -> FilePath   -- ^ Mount point
           -> IO ()
umountWith flag sym str =
    throwErrnoIfMinus1_ "umountWith" $
    withCString str $ \cstr ->
        c_umount2 cstr (fromUmountFlag flag .|. fromSymLink sym)

foreign import ccall unsafe "umount2"
  c_umount2 :: CString -> CInt -> IO CInt

-- | A filesystem independent option to be used when mounting a filesystem.
data MountFlag = ReadOnly     -- ^ Mount read-only (@MS_RDONLY@).
               | NoSUID       -- ^ Ignore suid and sgid bits (@MS_NOSUID@).
               | NoDev        -- ^ Disallow access to device special files
                              -- (@MS_NODEV@).
               | NoExec       -- ^ Disallow program execution (@MS_NOEXEC@).
               | Synchronous  -- ^ Writes are synced at once (@MS_SYNCHRONOUS@).
               | MandLock     -- ^ Allow mandatory locks on a filesystem
                              -- (@MS_MANDLOCK@).
               | DirSync      -- ^ Directory modifications are synchronous
                              -- (@MS_DIRSYNC@).
               | NoATime      -- ^ Do not update access times (@MS_NOATIME@).
               | NoDirATime   -- ^ Do not update directory access times
                              -- (@MS_NODIRATIME@).
               | Silent       -- ^ Silent mount (@MS_SILENT@).
               | PosixACL     -- ^ VFS does not apply the umask (@MS_POSIXACL@).
               | RelATime     -- ^ Update atime relative to mtime/ctime
                              -- (@MS_RELATIME@).
               | IVersion     -- ^ Update inode I_version field
                              -- (@MS_I_VERSION@).
               | StrictATime  -- ^ Always perform atime updates
                              -- (@MS_STRICTATIME@).
                 deriving (Eq, Read, Show)

fromMountFlag :: MountFlag -> CUInt
fromMountFlag ReadOnly    = #{const MS_RDONLY}
fromMountFlag NoSUID      = #{const MS_NOSUID}
fromMountFlag NoDev       = #{const MS_NODEV}
fromMountFlag NoExec      = #{const MS_NOEXEC}
fromMountFlag Synchronous = #{const MS_SYNCHRONOUS}
fromMountFlag MandLock    = #{const MS_MANDLOCK}
fromMountFlag DirSync     = #{const MS_DIRSYNC}
fromMountFlag NoATime     = #{const MS_NOATIME}
fromMountFlag NoDirATime  = #{const MS_NODIRATIME}
fromMountFlag Silent      = #{const MS_SILENT}
fromMountFlag PosixACL    = #{const MS_POSIXACL}
fromMountFlag RelATime    = #{const MS_RELATIME}
fromMountFlag IVersion    = #{const MS_I_VERSION}
fromMountFlag StrictATime = #{const MS_STRICTATIME}

-- | Filesystem dependent options to be used when mounting a filesystem; the
-- content of @'DriverData'@ is passed directly to the filesystem driver.
type DriverData = ByteString

-- | Empty @'DriverData'@.
noData :: DriverData
noData = empty

combineBitMasks :: [MountFlag] -> CULong
combineBitMasks = fromIntegral . foldl (.|.) 0 . map fromMountFlag

-- | A filesystem independent option to be used when unmounting a filesystem.
data UmountFlag = Plain  -- ^ Plain unmount, behaves like @'umount'@.
                | Force  -- ^ Force  unmount  even  if busy.
                | Detach -- ^ Perform a lazy unmount: make the mount point
                         -- unavailable for new accesses, and actually perform
                         -- the unmount when the mount point ceases to be busy.
                | Expire -- ^ Mark the mount point as expired. If a mount point
                         -- is not currently in use, then an initial call to
                         -- @'umountWith'@ with this flag fails with the error
                         -- @'Foreign.C.Error.eAGAIN'@, but marks the mount
                         -- point as expired. The mount point remains expired as
                         -- long as it isn't accessed by any process. A second
                         -- @'umountWith'@ call specifying @'Expire'@ unmounts
                         -- an expired mount point.
                  deriving (Eq, Read, Show)

fromUmountFlag :: UmountFlag -> CInt
fromUmountFlag Plain  = 0
fromUmountFlag Force  = #{const MNT_FORCE}
fromUmountFlag Detach = #{const MNT_DETACH}
fromUmountFlag Expire = #{const MNT_EXPIRE}

-- | Whether to follow symbolic links on umount.
data SymLink = Follow
             | NoFollow
               deriving (Eq, Read, Show)

fromSymLink :: SymLink -> CInt
fromSymLink Follow   = 0
fromSymLink NoFollow = #{const UMOUNT_NOFOLLOW}
