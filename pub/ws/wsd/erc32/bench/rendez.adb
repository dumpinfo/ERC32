
with text_io,calendar; use text_io,calendar;
procedure rendez is
a:integer:=0;
b:integer:=0;
start:time;
measured_time:duration;
n:integer;

package duratio_io is new fixed_io(duration);
use duratio_io;

package int_io is new integer_io(integer);
use int_io;

task ta is
 entry a1;
 entry a2;
 entry a3;
end ta;

task tb is
 entry b1;
end tb;

task body ta is
begin
 accept a1;
 while a<n loop
  tb.b1;
  accept a2;
  a:=a+1;
 end loop;
 accept a3;
end ta;

task body tb is
begin
 loop
  accept b1;
  ta.a2;
  b:=b+1;
 end loop;
end tb;

begin
-- put("desired number of rendez vous");
-- get(n); new_line;
-- n:=n/2; -- 2 rendez vous for each n in each loop below
 n := 200;
 put("Running test with ");
 put(integer'image(n));
 put_line(" rendez-vous");
 n:= n/2;
 start:=clock;
 ta.a1;
 ta.a3;
 measured_time:=clock-start;
 abort tb;
 put("used time = "); put(measured_time); put(" seconds"); new_line;
 put("a = "); put(integer'image(a));
 put("  b = "); put(integer'image(b)); new_line;
end rendez;
