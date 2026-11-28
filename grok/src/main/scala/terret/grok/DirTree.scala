package terret.grok

import zio.Chunk

import java.nio.file.{Files, LinkOption, Path}

import scala.jdk.CollectionConverters.*

/** Deletes a tree without following a symlink. The link node goes. The target stays. */
private[grok] object DirTree:
  def remove(path: Path): Unit =
    if Files.isSymbolicLink(path) then Files.deleteIfExists(path)
    else if Files.isDirectory(path, LinkOption.NOFOLLOW_LINKS) then
      val kids = Files.list(path)
      try Chunk.from(kids.iterator.asScala).foreach(remove)
      finally kids.close()
      Files.deleteIfExists(path)
    else Files.deleteIfExists(path)
end DirTree
