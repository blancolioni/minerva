with Athena;                           use Athena;

procedure Escape is
   Mass_Vector         : Athena.Vector;
   Total_Mass          : Float;
   Control_Vector      : Address with Address => 16#0006#;
   Command_Register    : Integer with Address => Control_Vector;
   Seek_Vector         : Athena.Vector with Address => Control_Vector + 4;
   Signal_Table_Vector : Address with Address => 16#0002#;
   Signal_Header       : Address with Address => Signal_Table_Vector;
   Hostile_Count       : Integer with Address => 16#4004#;
   Hostile_Addr        : Address with Address => 16#400A#;
   Hostile_Loop        : Address;
   Hostile_Index       : Integer;
begin
   loop
      Total_Mass   := 0.0;
      Mass_Vector  := Zero;
      Hostile_Loop := Hostile_Addr + 16#4000#;
      Hostile_Index := Hostile_Count;
      while Hostile_Index > 0 loop
         declare
            Hostile_Mass     : Float with Address => Hostile_Loop + 8;
            Hostile_Position : Athena.Vector with Address => Hostile_Loop + 12;
            Hostile_Vector   : Athena.Vector;
         begin
            Total_Mass := Total_Mass + Hostile_Mass;
            Hostile_Vector := Hostile_Mass * Hostile_Position;
            Mass_Vector := Mass_Vector + Hostile_Vector;
            Hostile_Index := Hostile_Index - 1;
         end;
      end loop;
      Seek_Vector := Zero;
      Command_Register := 1;
   end loop;      
end Escape;
