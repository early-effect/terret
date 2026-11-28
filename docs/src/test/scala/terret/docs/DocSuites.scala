package terret.docs

import specular.ziotest.DocSpecSuite

object RunSuite extends DocSpecSuite:
  def doc = Run.doc

object AttachSuite extends DocSpecSuite:
  def doc = Attach.doc

object JobsSuite extends DocSpecSuite:
  def doc = Jobs.doc

object EventsSuite extends DocSpecSuite:
  def doc = Events.doc

object CeilingSuite extends DocSpecSuite:
  def doc = CeilingPage.doc

object SecuritySuite extends DocSpecSuite:
  def doc = Security.doc

object ConfigSuite extends DocSpecSuite:
  def doc = Config.doc

object OperationsSuite extends DocSpecSuite:
  def doc = Operations.doc

object DshSuite extends DocSpecSuite:
  def doc = DshAdapter.doc
