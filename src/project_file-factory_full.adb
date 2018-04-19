----------------------------------------------------------------------
--  Project_File.Factory_Full - Package body                        --
--  Copyright (C) 2018 Adalog                                       --
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

with   -- Standard units
   Ada.Characters.Handling,
   Ada.Directories;

with   -- Reusable components
   Project_File.ADP,
   Project_File.Dummy,
   Project_File.GPR;

package body Project_File.Factory_Full is

   ADP_Project   : aliased ADP.Instance;
   Dummy_Project : aliased Dummy.Instance;
   GPR_Project   : aliased GPR.Instance;

   ---------------------------
   -- Corresponding_Project --
   ---------------------------

   function Corresponding_Project (Project_Name : String) return Project_File.Class_Access is
      use Ada.Characters.Handling, Ada.Directories;

   begin
      if Project_Name = "" then
         return Dummy_Project'Access;
      end if;

      declare
         Ext : constant String := To_Lower (Extension (Project_Name));
      begin
         if Ext = "" then -- assume GPR by default
            GPR_Project.Activate (Project_Name & ".gpr");
            return GPR_Project'Access;
         elsif Ext = "gpr" then
            GPR_Project.Activate (Project_Name);
            return GPR_Project'Access;
         elsif Ext = "adp" then
            ADP_Project.Activate (Project_Name);
            return ADP_Project'Access;
         else
            return Dummy_Project'Access;
         end if;
      end;
   end Corresponding_Project;

end Project_File.Factory_Full;
