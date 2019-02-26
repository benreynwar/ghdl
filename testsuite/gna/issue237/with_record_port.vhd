library ieee;
use ieee.std_logic_1164.all;

package with_record_port_pkg is
  type t_record_port is record
    subport_a : std_logic;
    subport_b : std_logic;
  end record;
end package;


library ieee;
use ieee.std_logic_1164.all;
use work.with_record_port_pkg.all;

entity with_record_port is
port (
   i_record_port : in  t_record_port;
   o_record_port : out t_record_port
);
end entity;

architecture rtl of with_record_port is
begin
  o_record_port <= i_record_port;
end architecture;


