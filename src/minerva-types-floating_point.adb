with Minerva.Primitives;

package body Minerva.Types.Floating_Point is

   Local_Standard_Float : Class_Reference;

   ------------
   -- Create --
   ------------

   function Create
     (Definition : not null access Minerva.Trees.Class;
      Name       : String;
      Num_Digits : Positive)
      return Class_Reference
   is
      Size : constant Minerva.Target.Object_Size :=
               (if Num_Digits <= 6 then 32 else 64);
      Result : Instance := Instance'
        (Parent with Num_Digits => Natural'Min (Num_Digits, 15));
   begin
      Result.Definition := Minerva.Trees.Class_Reference (Definition);
      Result.Name := Minerva.Names.To_Name (Name);
      Result.Object_Size := Size;
      Result.Is_Numeric := True;
      Result.Is_Ordered := True;
      Result.Is_Floating_Point := True;

      return new Instance'(Result);
   end Create;

   --------------------
   -- Standard_Float --
   --------------------

   function Standard_Float return Class_Reference is
   begin
      if Local_Standard_Float = null then
         Local_Standard_Float :=
           Create (Minerva.Primitives.Package_Standard,
                   "float",
                   Num_Digits => 6);
      end if;
      return Local_Standard_Float;
   end Standard_Float;

end Minerva.Types.Floating_Point;
