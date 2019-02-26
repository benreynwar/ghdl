#include <stdio.h>
#include <string.h>
#include <vpi_user.h>

struct net_descs
{
  const char *name;
  vpiHandle *handle;
};

static vpiHandle i_record_port, o_record_port;
static int cnt;

static struct net_descs nets[] = {
  { "with_record_port.i_record_port", &i_record_port},
  { "with_record_port.o_record_port", &o_record_port},
  { NULL, NULL}
};


static PLI_INT32
vpi_start_proc(p_cb_data data)
{
  s_vpi_value val;
  s_cb_data cb;
  int i;

  for (i = 0; nets[i].name; i++)
    {
      *nets[i].handle = vpi_handle_by_name ((char *)nets[i].name, NULL);
      if (*nets[i].handle == NULL)
        {
          vpi_printf ("Error: Cannot get net %s.\n", nets[i].name);
          return 0;
        }
    }
  return 0;
}

static void
my_handle_register(void)
{
  s_cb_data cb;

  cb.reason = cbStartOfSimulation;
  cb.cb_rtn = &vpi_start_proc;
  cb.user_data = NULL;
  if (vpi_register_cb (&cb) == NULL)
    vpi_printf ("Error: Cannot register EndStartOfSimulation call back.\n");
}

void (*vlog_startup_routines[]) () =
{
  my_handle_register,
  0
};
