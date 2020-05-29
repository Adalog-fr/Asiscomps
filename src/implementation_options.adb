----------------------------------------------------------------------
--  Implementation_Options - Package body                           --
--  Copyright (C) 2005-2016 Adalog                                  --
--  Author: J-P. Rosen                                              --
--                                                                  --
--  ADALOG   is   providing   training,   consultancy,   expertise, --
--  assistance and custom developments  in Ada and related software --
--  engineering techniques.  For more info about our services:      --
--  ADALOG                          Tel: +33 1 45 29 21 52          --
--  2 rue du Docteur Lombard        Fax: +33 1 45 29 25 00          --
--  92441 ISSY LES MOULINEAUX CEDEX E-m: info@adalog.fr             --
--  FRANCE                          URL: http://www.adalog.fr       --
--                                                                  --
--  This  unit is  free software;  you can  redistribute  it and/or --
--  modify  it under  terms of  the GNU  General Public  License as --
--  published by the Free Software Foundation; either version 2, or --
--  (at your  option) any later version.  This  unit is distributed --
--  in the hope  that it will be useful,  but WITHOUT ANY WARRANTY; --
--  without even the implied warranty of MERCHANTABILITY or FITNESS --
--  FOR A  PARTICULAR PURPOSE.  See the GNU  General Public License --
--  for more details.   You should have received a  copy of the GNU --
--  General Public License distributed  with this program; see file --
--  COPYING.   If not, write  to the  Free Software  Foundation, 59 --
--  Temple Place - Suite 330, Boston, MA 02111-1307, USA.           --
--                                                                  --
--  As  a special  exception, if  other files  instantiate generics --
--  from  this unit,  or you  link this  unit with  other  files to --
--  produce an executable,  this unit does not by  itself cause the --
--  resulting executable  to be covered  by the GNU  General Public --
--  License.  This exception does  not however invalidate any other --
--  reasons why  the executable  file might be  covered by  the GNU --
--  Public License.                                                 --
----------------------------------------------------------------------
with -- Standard Ada units
  Ada.Characters.Handling,
  Ada.Directories,
  Ada.Command_Line,
  Ada.Strings.Wide_Fixed,
  Ada.Strings.Wide_Unbounded;

with -- Adalog units
  Utilities;

package body Implementation_Options is

   -----------------------
   -- Initialize_String --
   -----------------------

   function Initialize_String (Debug_Mode : Boolean := False) return Wide_String is
      Default : constant Wide_String := "-ws -wv -k -asis05";
   begin
      if Debug_Mode then
         return Default;
      else
         return Default & " -nbb";   -- No Bug Box
      end if;
   end Initialize_String;

  -----------------------
   -- Parameters_String --
   -----------------------

   function Parameters_String (Project       : Project_File.Class_Access := null;
                               Other_Options : Wide_String := "") return Wide_String
   is
      use Ada.Strings.Wide_Fixed, Ada.Strings.Wide_Unbounded;
      use Project_File;

      Default_Options : Unbounded_Wide_String := Null_Unbounded_Wide_String;

   begin  -- Parameters_String
      if Index (Other_Options, "-C") = 0 then
         Append (Default_Options, "-C" & Default_C_Parameter);
      end if;

      if Index (Other_Options, "-F") = 0 then
         Append (Default_Options, " -F" & Default_F_Parameter);
      end if;

      -- Case of new ASIS: must provide the executable for asis-gcc
      Append (Default_Options, " --GCC=" & Tree_Generator);

      if Project /= null then  -- There is a project file
         Append (Default_Options, ' ' & Project.I_Options & ' ' & Project.T_Options);
      end if;

      return To_Wide_String (Default_Options) & ' ' & Other_Options;
   end Parameters_String;

   --------------------
   -- Tree_Generator --
   --------------------

   function Tree_Generator return Wide_String is
      use Ada.Characters.Handling, Ada.Command_Line, Ada.Directories;
      use Utilities;

      Asis_Gcc_Path   : constant String       := "/../libexec/asis-gnsa/bin/asis-gcc";

      function Command_Directory (Command : String) return String is
         Full_Path_Command : constant Wide_String := Locate_Regular_File (To_Wide_String (Command), "PATH");
      begin
         if Full_Path_Command = "" then -- not found
            return "";
         end if;
         return Containing_Directory (To_String (Full_Path_Command));
      end Command_Directory;
   begin  -- Tree_Generator
      -- We assume asis-gcc is installed in the same tree as AdaControl or in the same tree as gcc
      -- If not found, we assume it is the old ASIS technology, using regular gcc.
      if        Exists (Command_Directory (Command_Name) & Asis_Gcc_Path)
        or else Exists (Command_Directory (Command_Name) & Asis_Gcc_Path & ".exe")
      then
         return To_Wide_String (Command_Directory (Command_Name) & Asis_Gcc_Path);

      elsif     Exists (Command_Directory ("gcc") & Asis_Gcc_Path)
        or else Exists (Command_Directory ("gcc") & Asis_Gcc_Path & ".exe")
      then
         return To_Wide_String (Command_Directory ("gcc") & Asis_Gcc_Path);

      -- assume gcc
      elsif Locate_Regular_File ("gcc", "PATH") /= "" then
         return Locate_Regular_File ("gcc", "PATH");

      elsif Locate_Regular_File ("gcc.exe", "PATH") /= "" then
         return Locate_Regular_File ("gcc.exe", "PATH");

      else
         return "";
      end if;
   end Tree_Generator;

end Implementation_Options;
