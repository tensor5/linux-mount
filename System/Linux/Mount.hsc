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
-- This module provides an interface to the system mount and umount functions.
--
--------------------------------------------------------------------------------

module System.Linux.Mount
    ( -- * Bindings to system functions
      mount
    , umount
    , umountWith

    -- * Mount flags
    , MountFlag(..)

    -- * Driver data
    , DriverData
    , noData

    -- * Unmount flags
    , UmountFlag(..)
    , SymLink(..)

    ) where

#include <sys/mount.h>

import           Data.ByteString (ByteString, empty, useAsCString)
import           Foreign
import           Foreign.C

-- | Mount a filesystem (call to @mount()@).
mount :: String      -- ^ Device file
      -> FilePath    -- ^ Mount point
      -> String      -- ^ Filesystem type
      -> [MountFlag] -- ^ List of mount options
      -> DriverData  -- ^ Driver specific options
      -> IO ()
mount dev dir typ xs byt =
    throwErrnoIfMinus1_ "mount" $
    withCString dev $ \cdev ->
        withCString dir $ \cdir ->
            withCString typ $ \ctyp ->
                useAsCString byt $ \cdat->
                             c_mount cdev
                                     cdir
                                     ctyp
                                     (combineBitMasks xs)
                                     (castPtr cdat)

foreign import ccall unsafe "mount"
  c_mount :: CString -> CString -> CString -> CULong -> Ptr a -> IO CInt

-- | Unmount a filesystem (call to @umount()@).
umount :: FilePath -- ^ Mount point
       -> IO ()
umount str = throwErrnoIfMinus1_ "umount" (withCString str c_umount)

foreign import ccall unsafe "umount"
  c_umount :: CString -> IO CInt

-- | Unmount a filesystem using specific unmount options (call to @umount2()@).
-- See @'UmountFlag'@ for details.
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
data MountFlag = Rdonly
               | Nosuid
               | Nodev
               | Noexec
               | Synchronous
               | Remount
               | Mandlock
               | Dirsync
               | Noatime
               | Nodiratime
               | Bind
               | Move
               | Rec
               | Silent
               | Posixacl
               | Unbindable
               | Private
               | Slave
               | Shared
               | Relatime
               | Kernmount
               | IVersion
               | Strictatime
               | Active
               | Nouser
                 deriving (Eq, Read, Show)

fromMountFlag :: MountFlag -> CUInt
fromMountFlag Rdonly      = #{const MS_RDONLY}
fromMountFlag Nosuid      = #{const MS_NOSUID}
fromMountFlag Nodev       = #{const MS_NODEV}
fromMountFlag Noexec      = #{const MS_NOEXEC}
fromMountFlag Synchronous = #{const MS_SYNCHRONOUS}
fromMountFlag Remount     = #{const MS_REMOUNT}
fromMountFlag Mandlock    = #{const MS_MANDLOCK}
fromMountFlag Dirsync     = #{const MS_DIRSYNC}
fromMountFlag Noatime     = #{const MS_NOATIME}
fromMountFlag Nodiratime  = #{const MS_NODIRATIME}
fromMountFlag Bind        = #{const MS_BIND}
fromMountFlag Move        = #{const MS_MOVE}
fromMountFlag Rec         = #{const MS_REC}
fromMountFlag Silent      = #{const MS_SILENT}
fromMountFlag Posixacl    = #{const MS_POSIXACL}
fromMountFlag Unbindable  = #{const MS_UNBINDABLE}
fromMountFlag Private     = #{const MS_PRIVATE}
fromMountFlag Slave       = #{const MS_SLAVE}
fromMountFlag Shared      = #{const MS_SHARED}
fromMountFlag Relatime    = #{const MS_RELATIME}
fromMountFlag Kernmount   = #{const MS_KERNMOUNT}
fromMountFlag IVersion    = #{const MS_I_VERSION}
fromMountFlag Strictatime = #{const MS_STRICTATIME}
fromMountFlag Active      = #{const MS_ACTIVE}
fromMountFlag Nouser      = #{const MS_NOUSER}

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
                         -- @EAGAIN@, but marks the mount point as expired. The
                         -- mount point remains expired as long as it isn't
                         -- accessed by any process. A second @'umountWith'@
                         -- call specifying @'Expire'@ unmounts an expired mount
                         -- point.
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
