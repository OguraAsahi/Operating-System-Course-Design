//推荐范例代码
#include <linux/miscdevice.h>
#include <linux/kfifo.h>
#include <linux/wait.h>
#include <linux/sched.h>

#define DEMO_NAME "KFIFOBufferWithBlock"
#define BUFFER_SIZE 32
DEFINE_KFIFO(FIFO_Buffer,char,BUFFER_SIZE);

int ret = 0;
struct miscdevice miscDeviceFIFOBlock;
struct BlockFIFODevice BlockFIFODevice;

struct BlockFIFODevice{
	const char *name;
	struct device *device;
	struct miscdevice *miscDev;
	wait_queue_head_t read_queue;
	wait_queue_head_t write_queue;
};

static ssize_t DevRead(struct file *file ,char _user *buf,size_t count,loff_t *ppos)
{
	if(kfifo_is_empty(&FIFOBuffer))
	{
		ret = wait_event_interruptible(BlockFIFODevice->read_queue,
							!kfifo_is_empty(&FIFOBuffer));
	}
	
	ret = kfifo_to_user(&FIFOBuffer,buf,count,&actual_readed);
	
	if(!kfifo_is_full(&FIFOBuffer))
		wake_up_interruptible(BlockFIFODevice->write_queue);//写进程
}



static ssize_t DevWrite(struct file *file,const char _user *buf,size_t count, BlockFIFODevice *BlockFIFODevice)
{
	if (kfifo_is_full(&FIFOBuffer))
	{
		if (file->f_flags & O_NONBLOCK)
			return -EAGAIN;
			
		ret = wait_event_interruptible(BlockFIFODevice->write_queue,
										!kfifo_is_full(&FIFOBuffer));//使写进程等待，并设置唤醒条件
	}
	ret = kfifo_from_user(&FIFOBuff,buf,count,&actual_write);
	
	if(!kfifo_is_empty(&FIFOBuffer))
		wake_up_interruptible(&BlockFIFODevice->read_queue);//读进程
}


static int __init DevInit(void)
{
	ret = misc_register(&miscDeviceFIFOBlock);
	
	init_waitqueue_head(&BlockFIFODevice->read_queue);
	init_waitqueue_head(&BlockFIFODevice->write_queue);
}


