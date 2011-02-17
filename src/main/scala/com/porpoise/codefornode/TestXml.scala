package com.porpoise.codefornode

import scala.xml._

object TestXml {

   def xml = <books>
	   <book name="Oliver Twist">
		   <chapters>
			   <chapter name="Introduction">
				   <page title="page one">some content</page>
				   <page id="an id">some more content</page>
			   </chapter>
			   <chapter name="Conclusion">
				   <page footnote="some footnote" />
			   </chapter>
		   </chapters>
	   </book>
	   <book name="B2">
			<chapter name="Some chapter outside a chapters element">
				<page color="red">some content</page>
			</chapter>
	   </book>
   </books>
}