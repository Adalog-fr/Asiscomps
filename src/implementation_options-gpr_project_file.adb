----------------------------------------------------------------------
--  Implementation_Options.GPR_Project_File - Package body          --
--  Copyright (C) 2002-2016 Adalog                                  --
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
   Ada.Strings.Wide_Unbounded;

with -- GNAT units
   Gnat.Strings,
   Gnatcoll.Projects,
   Gnatcoll.VFS;

package body Implementation_Options.GPR_Project_File is

   --------------------
   -- Is_Appropriate --
   --------------------

   function Is_Appropriate (Project_Name : String) return Boolean is
      use Ada.Characters.Handling;
   begin
      return Project_Name'Length >= 5
        and then To_Upper (Project_Name (Project_Name'Last - 3 .. Project_Name'Last)) = ".GPR";
   end Is_Appropriate;

   ---------------
   -- I_Options --
   ---------------

   function I_Options (Project_Name : String) return Wide_String is
      use Ada.Characters.Handling, Ada.Strings.Wide_Unbounded;
      use Gnatcoll.Projects, Gnatcoll.VFS;

      Tree   : Project_Tree;
      Result : Unbounded_Wide_String;
   begin    -- I_Options_From_GPR_Project
      Load (Tree, Root_Project_Path => Create (+Project_Name));

      declare
         Project_Dirs : constant File_Array := Source_Dirs (Root_Project (Tree), Recursive => True);
      begin
         for D in Project_Dirs'Range loop
            Append (Result, " -I" & To_Wide_String (+Full_Name (Project_Dirs (D))));
         end loop;
      end;

      return To_Wide_String (Result);

   exception
      when Invalid_Project =>
         raise Implementation_Error with "Unknown or invalid GPR project: " & Project_Name;
   end I_Options;

   -----------------
   -- Tool_Switch --
   -----------------

   function Tool_Switch (Project_Name : String; Tool : String; After : String) return String is
      use Ada.Characters.Handling, Ada.Strings.Wide_Unbounded;
      use Gnatcoll.Projects, Gnatcoll.VFS;
      use type Gnat.Strings.String_List_Access;

      Tree   : Project_Tree;
   begin    -- I_Options_From_GPR_Project
      Load (Tree, Root_Project_Path => Create (+Project_Name));
      declare
         Attributes : constant GNAT.Strings.String_List_Access
           := Attribute_Value (Project      => Root_Project (Tree),
                               Attribute    => Build (Ide_Package, "Default_Switches"),
                               Index        => Tool,
                               Use_Extended => True);
      begin
         if Attributes = null then
            return "";
         end if;
         for I in Attributes'Range loop
            if Attributes (I).all = After then
               return Attributes (I+1).all;
            end if;
         end loop;
         return "";
      end;
   end Tool_Switch;
end Implementation_Options.GPR_Project_File;
