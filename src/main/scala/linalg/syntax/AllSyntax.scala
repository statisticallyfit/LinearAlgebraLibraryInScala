package linalg.syntax

//import spire.syntax.std.DoubleSyntax

/**
  *
  */
//note: no need to add IntSyntax etc here because the implicit classes in these below names
//note - account for taking int as their types - check TODO

//note it seems we can repeat! abeliansyntax even though already in numbersyntax
trait AllSyntax
     extends AbsoluteSyntax
     with RootSyntax
     with TrigSyntax
     with EqualitySyntax
     with NumberSyntax
     with NumericConversionSyntax
     with MonoidSyntax
     with AbelianGroupSyntax
     with RingSyntax
     with FieldSyntax
     with ShowSyntax //breaker between std stuff and vecspace stuff
     with DimensionSyntax
     with VectorSpaceSyntax
     with HilbertSpaceSyntax
     with InnerProductSpaceSyntax
     with NormedVectorSpaceSyntax
     with VectorLikeSyntax
     with SetVecLikeSyntax
     with MatrixLikeSyntax
     with LinearIndependenceSyntax
     with LinearSystemSyntax
     with SpanSyntax



