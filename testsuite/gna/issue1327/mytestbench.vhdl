library ieee ;

entity mytestbench is
end mytestbench;

architecture arch of mytestbench is
  signal sig : bit;
begin

  -- Just here so we get a meaningful dump.
  main_process: process
  begin
    wait for 10 ns;
    wait;
  end process;      

end arch; 
