package linalg.syntax

//import spire.syntax.std.DoubleSyntax

/**
  *
  */
//note: no need to add IntSyntax etc here because the implicit classes in these below names
//note - account for taking int as their types - check TODO

//note it seems we can repeat? abeliansyntax even though already in numbersyntax
trait AllSyntax
     extends AbsoluteSyntax
     with RootSyntax
     with TrigSyntax
     with EqualitySyntax
     with NumberSyntax
     //with RealNumberSyntax //note don't really need this? just extends numbersyntax
     with MonoidSyntax
     with AbelianGroupSyntax
     with RingSyntax
     with FieldSyntax
     with ShowSyntax //breaker between std stuff and vecspace stuff
     with DimensionSyntax


