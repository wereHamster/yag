module Git.Command (commandList) where

import qualified Git.Command.Add
import qualified Git.Command.Am
import qualified Git.Command.Annotate
import qualified Git.Command.Apply
import qualified Git.Command.Archimport
import qualified Git.Command.Archive
import qualified Git.Command.Bisect
import qualified Git.Command.Blame
import qualified Git.Command.Branch
import qualified Git.Command.Bundle
import qualified Git.Command.CatFile
import qualified Git.Command.CheckAttr
import qualified Git.Command.CheckRefFormat
import qualified Git.Command.Checkout
import qualified Git.Command.CheckoutIndex
import qualified Git.Command.Cherry
import qualified Git.Command.CherryPick
import qualified Git.Command.Citool
import qualified Git.Command.Clean
import qualified Git.Command.Clone
import qualified Git.Command.Commit
import qualified Git.Command.CommitTree
import qualified Git.Command.Config
import qualified Git.Command.CountObjects
import qualified Git.Command.Cvsexportcommit
import qualified Git.Command.Cvsimport
import qualified Git.Command.Cvsserver
import qualified Git.Command.Daemon
import qualified Git.Command.Describe
import qualified Git.Command.Diff
import qualified Git.Command.DiffFiles
import qualified Git.Command.DiffIndex
import qualified Git.Command.DiffTree
import qualified Git.Command.Difftool
import qualified Git.Command.FastExport
import qualified Git.Command.FastImport
import qualified Git.Command.Fetch
import qualified Git.Command.FetchPack
import qualified Git.Command.FilterBranch
import qualified Git.Command.FmtMergeMsg
import qualified Git.Command.ForEachRef
import qualified Git.Command.FormatPatch
import qualified Git.Command.Fsck
import qualified Git.Command.Gc
import qualified Git.Command.GetTarCommitId
import qualified Git.Command.Grep
import qualified Git.Command.Gui
import qualified Git.Command.HashObject
import qualified Git.Command.Help
import qualified Git.Command.HttpBackend
import qualified Git.Command.HttpFetch
import qualified Git.Command.HttpPush
import qualified Git.Command.ImapSend
import qualified Git.Command.IndexPack
import qualified Git.Command.Init
import qualified Git.Command.Instaweb
import qualified Git.Command.Log
import qualified Git.Command.LsFiles
import qualified Git.Command.LsRemote
import qualified Git.Command.LsTree
import qualified Git.Command.Mailinfo
import qualified Git.Command.Mailsplit
import qualified Git.Command.Merge
import qualified Git.Command.MergeBase
import qualified Git.Command.MergeFile
import qualified Git.Command.MergeIndex
import qualified Git.Command.MergeOneFile
import qualified Git.Command.MergeTree
import qualified Git.Command.Mergetool
import qualified Git.Command.Mktag
import qualified Git.Command.Mktree
import qualified Git.Command.Mv
import qualified Git.Command.NameRev
import qualified Git.Command.Notes
import qualified Git.Command.PackObjects
import qualified Git.Command.PackRedundant
import qualified Git.Command.PackRefs
import qualified Git.Command.ParseRemote
import qualified Git.Command.PatchId
import qualified Git.Command.Prune
import qualified Git.Command.PrunePacked
import qualified Git.Command.Pull
import qualified Git.Command.Push
import qualified Git.Command.Quiltimport
import qualified Git.Command.ReadTree
import qualified Git.Command.Rebase
import qualified Git.Command.ReceivePack
import qualified Git.Command.Reflog
import qualified Git.Command.Relink
import qualified Git.Command.Remote
import qualified Git.Command.Repack
import qualified Git.Command.Replace
import qualified Git.Command.RequestPull
import qualified Git.Command.Rerere
import qualified Git.Command.Reset
import qualified Git.Command.RevList
import qualified Git.Command.RevParse
import qualified Git.Command.Revert
import qualified Git.Command.Rm
import qualified Git.Command.SendEmail
import qualified Git.Command.SendPack
import qualified Git.Command.ShSetup
import qualified Git.Command.Shell
import qualified Git.Command.Shortlog
import qualified Git.Command.Show
import qualified Git.Command.ShowBranch
import qualified Git.Command.ShowIndex
import qualified Git.Command.ShowRef
import qualified Git.Command.Stash
import qualified Git.Command.Status
import qualified Git.Command.Stripspace
import qualified Git.Command.Submodule
import qualified Git.Command.Svn
import qualified Git.Command.SymbolicRef
import qualified Git.Command.Tag
import qualified Git.Command.UnpackFile
import qualified Git.Command.UnpackObjects
import qualified Git.Command.UpdateIndex
import qualified Git.Command.UpdateRef
import qualified Git.Command.UpdateServerInfo
import qualified Git.Command.UploadArchive
import qualified Git.Command.UploadPack
import qualified Git.Command.Var
import qualified Git.Command.VerifyPack
import qualified Git.Command.VerifyTag
import qualified Git.Command.Whatchanged
import qualified Git.Command.WriteTree


commandList =
  [ ("add",                  Git.Command.Add.run                 )
  , ("am",                   Git.Command.Am.run                  )
  , ("annotate",             Git.Command.Annotate.run            )
  , ("apply",                Git.Command.Apply.run               )
  , ("archimport",           Git.Command.Archimport.run          )
  , ("archive",              Git.Command.Archive.run             )
  , ("bisect",               Git.Command.Bisect.run              )
  , ("blame",                Git.Command.Blame.run               )
  , ("branch",               Git.Command.Branch.run              )
  , ("bundle",               Git.Command.Bundle.run              )
  , ("cat-file",             Git.Command.CatFile.run             )
  , ("check-attr",           Git.Command.CheckAttr.run           )
  , ("check-ref-format",     Git.Command.CheckRefFormat.run      )
  , ("checkout",             Git.Command.Checkout.run            )
  , ("checkout-index",       Git.Command.CheckoutIndex.run       )
  , ("cherry",               Git.Command.Cherry.run              )
  , ("cherry-pick",          Git.Command.CherryPick.run          )
  , ("citool",               Git.Command.Citool.run              )
  , ("clean",                Git.Command.Clean.run               )
  , ("clone",                Git.Command.Clone.run               )
  , ("commit",               Git.Command.Commit.run              )
  , ("commit-tree",          Git.Command.CommitTree.run          )
  , ("config",               Git.Command.Config.run              )
  , ("count-objects",        Git.Command.CountObjects.run        )
  , ("cvsexportcommit",      Git.Command.Cvsexportcommit.run     )
  , ("cvsimport",            Git.Command.Cvsimport.run           )
  , ("cvsserver",            Git.Command.Cvsserver.run           )
  , ("daemon",               Git.Command.Daemon.run              )
  , ("describe",             Git.Command.Describe.run            )
  , ("diff",                 Git.Command.Diff.run                )
  , ("diff-files",           Git.Command.DiffFiles.run           )
  , ("diff-index",           Git.Command.DiffIndex.run           )
  , ("diff-tree",            Git.Command.DiffTree.run            )
  , ("difftool",             Git.Command.Difftool.run            )
  , ("fast-export",          Git.Command.FastExport.run          )
  , ("fast-import",          Git.Command.FastImport.run          )
  , ("fetch",                Git.Command.Fetch.run               )
  , ("fetch-pack",           Git.Command.FetchPack.run           )
  , ("filter-branch",        Git.Command.FilterBranch.run        )
  , ("fmt-merge-msg",        Git.Command.FmtMergeMsg.run         )
  , ("for-each-ref",         Git.Command.ForEachRef.run          )
  , ("format-patch",         Git.Command.FormatPatch.run         )
  , ("fsck",                 Git.Command.Fsck.run                )
  , ("gc",                   Git.Command.Gc.run                  )
  , ("get-tar-commit-id",    Git.Command.GetTarCommitId.run      )
  , ("grep",                 Git.Command.Grep.run                )
  , ("gui",                  Git.Command.Gui.run                 )
  , ("hash-object",          Git.Command.HashObject.run          )
  , ("help",                 Git.Command.Help.run                )
  , ("http-backend",         Git.Command.HttpBackend.run         )
  , ("http-fetch",           Git.Command.HttpFetch.run           )
  , ("http-push",            Git.Command.HttpPush.run            )
  , ("imap-send",            Git.Command.ImapSend.run            )
  , ("index-pack",           Git.Command.IndexPack.run           )
  , ("init",                 Git.Command.Init.run                )
  , ("instaweb",             Git.Command.Instaweb.run            )
  , ("log",                  Git.Command.Log.run                 )
  , ("ls-files",             Git.Command.LsFiles.run             )
  , ("ls-remote",            Git.Command.LsRemote.run            )
  , ("ls-tree",              Git.Command.LsTree.run              )
  , ("mailinfo",             Git.Command.Mailinfo.run            )
  , ("mailsplit",            Git.Command.Mailsplit.run           )
  , ("merge",                Git.Command.Merge.run               )
  , ("merge-base",           Git.Command.MergeBase.run           )
  , ("merge-file",           Git.Command.MergeFile.run           )
  , ("merge-index",          Git.Command.MergeIndex.run          )
  , ("merge-one-file",       Git.Command.MergeOneFile.run        )
  , ("merge-tree",           Git.Command.MergeTree.run           )
  , ("mergetool",            Git.Command.Mergetool.run           )
  , ("mktag",                Git.Command.Mktag.run               )
  , ("mktree",               Git.Command.Mktree.run              )
  , ("mv",                   Git.Command.Mv.run                  )
  , ("name-rev",             Git.Command.NameRev.run             )
  , ("notes",                Git.Command.Notes.run               )
  , ("pack-objects",         Git.Command.PackObjects.run         )
  , ("pack-redundant",       Git.Command.PackRedundant.run       )
  , ("pack-refs",            Git.Command.PackRefs.run            )
  , ("parse-remote",         Git.Command.ParseRemote.run         )
  , ("patch-id",             Git.Command.PatchId.run             )
  , ("prune",                Git.Command.Prune.run               )
  , ("prune-packed",         Git.Command.PrunePacked.run         )
  , ("pull",                 Git.Command.Pull.run                )
  , ("push",                 Git.Command.Push.run                )
  , ("quiltimport",          Git.Command.Quiltimport.run         )
  , ("read-tree",            Git.Command.ReadTree.run            )
  , ("rebase",               Git.Command.Rebase.run              )
  , ("receive-pack",         Git.Command.ReceivePack.run         )
  , ("reflog",               Git.Command.Reflog.run              )
  , ("relink",               Git.Command.Relink.run              )
  , ("remote",               Git.Command.Remote.run              )
  , ("repack",               Git.Command.Repack.run              )
  , ("replace",              Git.Command.Replace.run             )
  , ("request-pull",         Git.Command.RequestPull.run         )
  , ("rerere",               Git.Command.Rerere.run              )
  , ("reset",                Git.Command.Reset.run               )
  , ("rev-list",             Git.Command.RevList.run             )
  , ("rev-parse",            Git.Command.RevParse.run            )
  , ("revert",               Git.Command.Revert.run              )
  , ("rm",                   Git.Command.Rm.run                  )
  , ("send-email",           Git.Command.SendEmail.run           )
  , ("send-pack",            Git.Command.SendPack.run            )
  , ("sh-setup",             Git.Command.ShSetup.run             )
  , ("shell",                Git.Command.Shell.run               )
  , ("shortlog",             Git.Command.Shortlog.run            )
  , ("show",                 Git.Command.Show.run                )
  , ("show-branch",          Git.Command.ShowBranch.run          )
  , ("show-index",           Git.Command.ShowIndex.run           )
  , ("show-ref",             Git.Command.ShowRef.run             )
  , ("stash",                Git.Command.Stash.run               )
  , ("status",               Git.Command.Status.run              )
  , ("stripspace",           Git.Command.Stripspace.run          )
  , ("submodule",            Git.Command.Submodule.run           )
  , ("svn",                  Git.Command.Svn.run                 )
  , ("symbolic-ref",         Git.Command.SymbolicRef.run         )
  , ("tag",                  Git.Command.Tag.run                 )
  , ("unpack-file",          Git.Command.UnpackFile.run          )
  , ("unpack-objects",       Git.Command.UnpackObjects.run       )
  , ("update-index",         Git.Command.UpdateIndex.run         )
  , ("update-ref",           Git.Command.UpdateRef.run           )
  , ("update-server-info",   Git.Command.UpdateServerInfo.run    )
  , ("upload-archive",       Git.Command.UploadArchive.run       )
  , ("upload-pack",          Git.Command.UploadPack.run          )
  , ("var",                  Git.Command.Var.run                 )
  , ("verify-pack",          Git.Command.VerifyPack.run          )
  , ("verify-tag",           Git.Command.VerifyTag.run           )
  , ("whatchanged",          Git.Command.Whatchanged.run         )
  , ("write-tree",           Git.Command.WriteTree.run           )
  ]