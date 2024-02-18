package body Minerva.Types.Universal is

   Local_Universal_Character : Class_Reference;
   Local_Universal_Integer   : Class_Reference;
   Local_Universal_Float     : Class_Reference;

   type Universal_Type_Instance is
     new Instance with null record;

   overriding function Description
     (This : Universal_Type_Instance)
      return String
   is (if This.Is_Integral
       then "universal integer"
       else "universal float");

   overriding function Is_Convertible_To
     (From : not null access constant Universal_Type_Instance;
      To   : not null access constant Class)
      return Boolean;

   -----------------------
   -- Is_Convertible_To --
   -----------------------

   overriding function Is_Convertible_To
     (From : not null access constant Universal_Type_Instance;
      To   : not null access constant Class)
      return Boolean
   is
   begin
      if From.Is_Integral then
         return To.Is_Integral;
      elsif From.Is_Floating_Point then
         return To.Is_Floating_Point;
      elsif From.Is_Enumerated then
         return To.Is_Enumerated;
      else
         return False;
      end if;
   end Is_Convertible_To;

   -------------------------
   -- Universal_Character --
   -------------------------

   function Universal_Character return Class_Reference is
   begin
      if Local_Universal_Character = null then
         Local_Universal_Character :=
           new Universal_Type_Instance'
             (Definition        => <>,
              Name              =>
                Minerva.Names.To_Name ("universal-integer"),
              Object_Size       => 0,
              Is_Composite      => False,
              Is_Enumerated     => True,
              Is_Floating_Point => False,
              Is_Integral       => False,
              Is_Limited        => False,
              Is_Numeric        => False,
              Is_Ordered        => True,
              Is_Unsigned       => False,
              Is_Signed         => False,
              Is_Universal      => True);
      end if;
      return Local_Universal_Character;
   end Universal_Character;

   ---------------------
   -- Universal_Float --
   ---------------------

   function Universal_Float return Class_Reference is
   begin
      if Local_Universal_Float = null then
         Local_Universal_Float :=
           new Universal_Type_Instance'
             (Definition        => <>,
              Name              =>
                Minerva.Names.To_Name ("universal-float"),
              Object_Size       => 0,
              Is_Composite      => False,
              Is_Enumerated     => False,
              Is_Floating_Point => True,
              Is_Integral       => False,
              Is_Limited        => False,
              Is_Numeric        => True,
              Is_Ordered        => True,
              Is_Unsigned       => False,
              Is_Signed         => False,
              Is_Universal      => True);
      end if;
      return Local_Universal_Float;
   end Universal_Float;

   -----------------------
   -- Universal_Integer --
   -----------------------

   function Universal_Integer return Class_Reference is
   begin
      if Local_Universal_Integer = null then
         Local_Universal_Integer :=
           new Universal_Type_Instance'
             (Definition        => <>,
              Name              =>
                Minerva.Names.To_Name ("universal-integer"),
              Object_Size       => 0,
              Is_Composite      => False,
              Is_Enumerated     => False,
              Is_Floating_Point => False,
              Is_Integral       => True,
              Is_Limited        => False,
              Is_Numeric        => True,
              Is_Ordered        => True,
              Is_Unsigned       => True,
              Is_Signed         => True,
              Is_Universal      => True);
      end if;
      return Local_Universal_Integer;
   end Universal_Integer;

end Minerva.Types.Universal;
