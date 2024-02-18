package body Minerva.Types.Composite is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This                  : in out Class;
      Definition            : not null access Minerva.Trees.Class;
      Object_Size           : Minerva.Target.Object_Size;
      Component_Environment : Minerva.Ids.Environment_Id)
   is
   begin
      This.Definition := Minerva.Trees.Class_Reference (Definition);
      This.Object_Size := Object_Size;
      This.Is_Composite := True;
      This.Components := Component_Environment;
   end Initialize;

end Minerva.Types.Composite;
