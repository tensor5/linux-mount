{-# LANGUAGE ForeignFunctionInterface #-}

-- | This module provides an interface to the system mount and umount
-- functions.
module System.Linux.Mount
    ( -- * Bindings to system functions
      mount
    , umount
    , umountWith

    -- *Mount flags
    , MountFlag(..)

    -- *Driver data
    , DriverData
    , noData

    -- *Unmount flags
    , UmountFlag(..)

    ) where

#include <sys/mount.h>

import Data.Bits
import Data.ByteString (ByteString, useAsCString, empty)
import Foreign
import Foreign.C

-- | Mount a filesystem (call to @mount()@).
mount :: String        -- ^ Device file
      -> FilePath      -- ^ Mount point
      -> String        -- ^ Filesystem type
      -> [MountFlag]   -- ^ List of mount options
      -> DriverData    -- ^ Driver specific options
      -> IO ()
mount dev dir typ xs byt =
    throwErrnoIfMinus1_ "mount" $ do
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
  c_mount :: Ptr CChar -> Ptr CChar -> Ptr CChar -> CULong -> Ptr a -> IO CInt

-- | Unmount a filesystem (call to @umount()@).
umount :: FilePath -- ^ Mount point
       -> IO ()
umount str = throwErrnoIfMinus1_ "umount"
             (withCString str (\cstr -> (c_umount cstr)))

foreign import ccall unsafe "umount"
  c_umount :: Ptr CChar -> IO CInt

-- | Unmount a filesystem using specific unmount options (call to
-- @umount2()@). See @'UmountFlag'@ for details.
umountWith :: UmountFlag -- ^ Unmount option
           -> Bool       -- ^ @'True'@ follow symbolic links,
                         -- @'False'@ do not follow
           -> FilePath   -- ^ Mount point
           -> IO ()
umountWith flag b str =
    throwErrnoIfMinus1_ "umountWith" $
    withCString str $ \cstr ->
        c_umount2 cstr (fromUmountFlag flag .|.
                        (if b then 0 else #{const UMOUNT_NOFOLLOW})
                       )



foreign import ccall unsafe "umount2"
  c_umount2 :: Ptr CChar -> CInt -> IO CInt

-- | A filesystem independent option to be used when mounting a
-- filesystem.
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

fromMountFlag :: MountFlag -> CULong
fromMountFlag Rdonly = fromIntegral msRdonly
fromMountFlag Nosuid = fromIntegral msNosuid
fromMountFlag Nodev = fromIntegral msNodev
fromMountFlag Noexec = fromIntegral msNoexec
fromMountFlag Synchronous = fromIntegral msSynchronous
fromMountFlag Remount = fromIntegral msRemount
fromMountFlag Mandlock = fromIntegral msMandlock
fromMountFlag Dirsync = fromIntegral msDirsync
fromMountFlag Noatime = fromIntegral msNoatime
fromMountFlag Nodiratime = fromIntegral msNodiratime
fromMountFlag Bind = fromIntegral msBind
fromMountFlag Move = fromIntegral msMove
fromMountFlag Rec = fromIntegral msRec
fromMountFlag Silent = fromIntegral msSilent
fromMountFlag Posixacl = fromIntegral msPosixacl
fromMountFlag Unbindable = fromIntegral msUnbindable
fromMountFlag Private = fromIntegral msPrivate
fromMountFlag Slave = fromIntegral msSlave
fromMountFlag Shared = fromIntegral msShared
fromMountFlag Relatime = fromIntegral msRelatime
fromMountFlag Kernmount = fromIntegral msKernmount
fromMountFlag IVersion = fromIntegral msIVersion
fromMountFlag Strictatime = fromIntegral msStrictatime
fromMountFlag Active = fromIntegral msActive
fromMountFlag Nouser = fromIntegral msNouser

-- | Filesystem dependent options to be used when mounting a
-- filesystem; the content of @'DriverData'@ is passed directly to the
-- filesystem driver.
type DriverData = ByteString

-- | Empty @'DriverData'@.
noData :: DriverData
noData = empty

combineBitMasks :: [MountFlag] -> CULong
combineBitMasks = foldl (.|.) 0 . map (fromMountFlag)

-- | A filesystem independent option to be used when unmounting a
-- filesystem.
data UmountFlag = Plain  -- ^ Plain unmount, behaves like @'umount'@.
                | Force  -- ^ Force  unmount  even  if busy.
                | Detach -- ^ Perform a lazy unmount: make the mount
                         -- point unavailable for new accesses, and
                         -- actually perform the unmount when the
                         -- mount point ceases to be busy.
                | Expire -- ^ Mark the mount point as expired. If a
                         -- mount point is not currently in use, then
                         -- an initial call to @'umountWith'@ with
                         -- this flag fails with the error @EAGAIN@, but
                         -- marks the mount point as expired. The
                         -- mount point remains expired as long as it
                         -- isn't accessed by any process. A second
                         -- @'umountWith'@ call specifying @'Expire'@
                         -- unmounts an expired mount point.
                  deriving (Eq, Read, Show)

fromUmountFlag :: UmountFlag -> CInt
fromUmountFlag Plain = 0
fromUmountFlag Force = #{const MNT_FORCE}
fromUmountFlag Detach = #{const MNT_DETACH}
fromUmountFlag Expire = #{const MNT_EXPIRE}

#enum CULong, , MS_RDONLY, MS_NOSUID, MS_NODEV, MS_NOEXEC, MS_SYNCHRONOUS, MS_REMOUNT, MS_MANDLOCK, MS_DIRSYNC, MS_NOATIME, MS_NODIRATIME, MS_BIND, MS_MOVE, MS_REC, MS_SILENT, MS_POSIXACL, MS_UNBINDABLE, MS_PRIVATE, MS_SLAVE, MS_SHARED, MS_RELATIME, MS_KERNMOUNT, MS_I_VERSION, MS_STRICTATIME, MS_ACTIVE, MS_NOUSER