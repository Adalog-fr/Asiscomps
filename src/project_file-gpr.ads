----------------------------------------------------------------------
--  Project_File.GPR - Package specification                        --
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


private with -- GNAT units
   Gnatcoll.Projects;

package Project_File.GPR is
   -- Abstraction of a Gnat .gpr project file

   type Instance is new Project_File.Instance with private;

   procedure Activate (Project : access GPR.Instance; Name : String);

   overriding function I_Options (Project : access GPR.Instance) return Wide_String;
   -- Constructs a list of -I<name> options from the source_dirs indications

   overriding function T_Options (Project : access GPR.Instance) return Wide_String;
   -- Constructs a list of -T<name> options from the object_dir indications

   overriding function Tool_Switch (Project : access GPR.Instance;
                                    Tool    : String;
                                    After   : String) return String;
   -- From Default_Switches of (GPR) package IDE:
   -- returns the value of the parameter that follows After, or "" if not found

   overriding function Tool_Switch_Present (Project : access GPR.Instance;
                                            Tool    : String;
                                            Switch  : String) return Boolean;
   -- From Default_Switches of (GPR) package IDE:
   -- returns True if the switch Switch is given

   overriding function Main_Files (Project : access GPR.Instance) return Names_List;
   -- Returns a space-separated list of declared main files

private
   type Instance is new Project_File.Instance with
      record
         Tree : GNATCOLL.Projects.Project_Tree;
      end record;

end Project_File.GPR;
