with Minerva.Trees.Sequences;
with Minerva.Trees.Statements;

package Minerva.Trees.Statement_Sequences is
  new Minerva.Trees.Sequences
    (Element_Instance  => Minerva.Trees.Statements.Instance,
     Element_Reference => Minerva.Trees.Statements.Class_Reference);
