package Minerva.Build is

   procedure Run_Build
     (Source_Path : String;
      Success     : out Boolean);

   procedure Build_Main
     (Main_File : String);

end Minerva.Build;
