package terret.docs

import specular.ziotest.DocSpecSuite

object AttachSuite extends DocSpecSuite:
  def doc = Attach.doc

object JobsSuite extends DocSpecSuite:
  def doc = Jobs.doc

object CeilingSuite extends DocSpecSuite:
  def doc = CeilingPage.doc

object ConfigSuite extends DocSpecSuite:
  def doc = Config.doc

object DshSuite extends DocSpecSuite:
  def doc = DshAdapter.doc
