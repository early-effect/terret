package terret.grok

import zio.{UIO, ZIO}

trait HostAllowlist:
  def load: UIO[NetworkPolicy]

object HostAllowlist:
  val closed: HostAllowlist = fixed(NetworkPolicy.empty)

  def fixed(policy: NetworkPolicy): HostAllowlist = new:
    def load: UIO[NetworkPolicy] = ZIO.succeed(policy)
end HostAllowlist
