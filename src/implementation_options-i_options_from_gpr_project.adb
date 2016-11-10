----------------------------------------------------------------------
--  Implementation_Options.I_Options_From_GPR_Project - func. body  --
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
with -- GNAT units
  Gnatcoll.Projects,
  Gnatcoll.VFS;

separate (Implementation_Options)
function I_Options_From_GPR_Project (Project_File : String) return Wide_String is
   use Ada.Characters.Handling, Ada.Strings.Wide_Unbounded;
   use Gnatcoll.Projects, Gnatcoll.VFS;

   Tree   : Project_Tree;
   Result : Unbounded_Wide_String;
begin    -- I_Options_From_GPR_Project
   Load (Tree, Root_Project_Path => Create (+Project_File));

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
      raise Implementation_Error with "Unknown or invalid GPR project: " & Project_File;
end I_Options_From_GPR_Project;
