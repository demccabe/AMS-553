import numpy as np
import time
import statistics as stat
import matplotlib.pyplot as plt


class Inventory:
    def __init__(self, capacity, policy, alpha=0.5, threshold=50, init_stock=0, order_max=100, order_min=0, order_cost_avg=100, order_fix=30, hold_cost=.3, back_cost=20,
                    pre_gen_cus_order=None, pre_gen_unit_cost=None):

        self.capacity = capacity
        self.threshold = threshold
        self.order_policy = policy

        # random generated variables
        ## customer's order parameters
        self.customer_order = 0
        self.customer_order_mean = 20
        self.customer_order_std = 2
        ## unit cost of order
        self.order_u_cost_avg = order_cost_avg
        self.order_u_cost = self.order_u_cost_avg
        self.order_u_cost_aplha = alpha
        self.order_u_cost_noise_mean = 0
        self.order_u_cost_noise_std = 1
        ## use pregeneraged variable if provided
        self.pre_cus_order = pre_gen_cus_order
        self.pre_unit_cost = pre_gen_unit_cost

        # variables
        self.cost = 0
        self.order = 0
        self.cur_stock = init_stock

        # order parameters
        self.order_max = order_max
        self.order_min = order_min
        self.order_fixed_fee = order_fix # fixed cost per order
        self.actual_order_cost = 0

        # other cost constant
        self.hold_u_cost = hold_cost
        self.backlog_u_cost = back_cost

        # log
        self.stock_log = []
        self.order_log = []
        self.penalty_log = []
        self.cost_log = []
        self.unit_cost_log = []
        self.customer_order_log = []

    def run(self):
        self.cur_stock += self.order
        self.generate_customer_order()
        self.update_stock()
        self.generate_order()
        self.update_cost()
        self.update_log()

    def update_log(self):
        self.stock_log.append(self.cur_stock)
        self.order_log.append(self.order)
        self.cost_log.append(self.cost)
        self.unit_cost_log.append(self.order_u_cost)
        self.customer_order_log.append(self.customer_order)

    def update_stock(self):
        self.cur_stock -= self.customer_order
        if self.cur_stock < 0:
            self.cur_penalty = self.backlog_u_cost * abs(self.cur_stock)
            #self.cur_stock = 0

    def update_cost(self):
        self.actual_order_cost = self.order_fixed_fee + self.order_u_cost * self.order
        self.holding_cost = self.hold_u_cost * self.cur_stock if self.cur_stock > 0 else 0
        self.backlog_cost = self.backlog_u_cost * self.cur_stock if self.cur_stock < 0 else 0
        self.cost = self.actual_order_cost + self.hold_u_cost + self.backlog_cost
        # update unit cost for next round
        if self.pre_unit_cost is None:
            self.order_u_cost = self.order_u_cost + self.order_u_cost_aplha * (self.order_u_cost_avg - self.order_u_cost) + np.random.normal(self.order_u_cost_noise_mean, self.order_u_cost_noise_std)
        else:
            self.order_u_cost = self.pre_unit_cost[0]
            self.pre_unit_cost = self.pre_unit_cost[1:]

    def generate_customer_order(self, mean=20, std=2):
        'generate customer order number'
        #self.customer = np.random.randint(min, max + 1)
        if self.pre_cus_order is None:
            self.customer_order = max(0, np.floor(np.random.normal(mean, std)))
        else:
            self.customer_order = self.pre_cus_order[0]
            self.pre_cus_order = self.pre_cus_order[1:]

    def potential_cost(self, stock, next_cus_order, unit_cost, backlog):
        ''' return if refill at t+1 cost > refill t cost
            if not refill:
            hold_cost * stock(t) + unit_cost(t+1) * (capacity - stock(t) + customer_order(t+1)) + backlog(t+1) * backlog_u_cost

            if refill now:
            hold_cost * capacity + unit_cost(t) * (capacity - stock(t))
            '''
        return self.hold_u_cost * stock + unit_cost * (self.capacity - self.cur_stock + next_cus_order) * unit_cost + self.backlog_u_cost * backlog

    def direct_refill(self):
        #self.order = min(self.order_max - self.cur_stock, 100)
        self.order = self.capacity - self.cur_stock

    def improved_refill(self, sims=100):
        order_u_cost_avg = np.mean(self.unit_cost_log + [self.order_u_cost])
        order_u_cost_std = np.sqrt(stat.variance(self.unit_cost_log + [self.order_u_cost], order_u_cost_avg))

        cus_order_avg = np.mean(self.customer_order_log + [self.customer_order])
        cus_order_std = np.sqrt(stat.variance(self.customer_order_log + [self.customer_order], cus_order_avg))

        # simulating unit cost, customer order number and calculate average backlog
        foo_unit_cost = self.order_u_cost + self.order_u_cost_aplha * (order_u_cost_avg - self.order_u_cost) + np.random.normal(0, order_u_cost_std, sims)
        foo_unit_cost_mean = np.mean(foo_unit_cost)

        foo_cus_order = np.maximum(0, np.floor(np.random.normal(cus_order_avg, cus_order_std, sims)))
        foo_cus_order_mean = np.mean(foo_cus_order)

        foo_backlog_mean = np.sum(self.cur_stock - foo_cus_order[self.cur_stock - foo_cus_order < 0]) / sims
        '''
        foo_unit_cost = np.zeros(100)
        foo_cus_order = np.zeros(100)
        foo_backlog = np.zeros(100)
        for i in range(100):
            foo_unit_cost[i] = self.order_u_cost + self.order_u_cost_aplha * (order_u_cost_avg - self.order_u_cost) + np.random.normal(self.order_u_cost_noise_mean, self.order_u_cost_noise_std)
            foo_cus_order[i] = max(0, np.floor(np.random.normal(self.customer_order_mean, self.customer_order_std)))
            foo_backlog[i] = foo_cus_order[i] - self.cur_stock if foo_cus_order[i] > self.cur_stock else 0
        foo_unit_cost_mean = np.mean(foo_unit_cost)
        foo_cus_order_mean = np.mean(foo_cus_order)
        foo_backlog_mean = np.mean(foo_backlog)
        '''

        # if refill next round > refill now
        if self.potential_cost(self.cur_stock, foo_cus_order_mean, foo_unit_cost_mean, foo_backlog_mean) > \
            self.potential_cost(self.capacity, cus_order_avg, self.order_u_cost, 0):
        #    self.potential_cost(self.capacity, foo_cus_order_mean, self.order_u_cost, 0):
            #self.potential_cost(self.capacity, cus_order_avg, self.order_u_cost, 0):
            self.order = self.capacity - self.cur_stock
        else:
            #print("wait for next round")
            self.order = 0

    def generate_order(self):
        'generate order for restock inventory'
        if self.order_policy == "improved":
            if len(self.cost_log) > 1:
                self.improved_refill()
            else:
                self.direct_refill()
        elif self.order_policy == "direct":
            if self.cur_stock < self.threshold:
                self.direct_refill()
            else:
                self.order = 0
        else:
            self.order = 0

if __name__ == "__main__":
    plt.close('all')
    avg_cost_direct = []
    avg_cost_improved = []
    avg_stock_direct = []
    avg_stock_improved = []
    avg_order_direct = []
    avg_order_improved = []
    #threshold = 100

    paras = {
        'init_stock' : 100,
        'alpha' : 0.5,
       # 'order_fix' : 30,
        'order_cost_avg' : 100,
        'hold_cost' : 0.3,
        'back_cost' : 20
        }

    start = time.time()
    #for threshold in range(10, 90):
    ticks = np.arange(10, 101, 10)
    for order_fix in ticks:
        threshold = 17
        direc_avg_cost = []
        improved_avg_cost = []
        for avg in range(100):
            inv = Inventory(capacity=100, policy="direct", threshold=threshold, order_fix=order_fix, **paras)
            for i in range(200):
                inv.run()
            direc_avg_cost.append(np.mean(inv.cost_log))

            inv2 = Inventory(capacity=100, policy="improved", threshold=threshold, order_fix=order_fix, pre_gen_cus_order=inv.customer_order_log, pre_gen_unit_cost=inv.unit_cost_log, **paras)
            #inv2 = Inventory(100, policy="improved", threshold=threshold, alpha=0.5, init_stock=100)
            for i in range(200):
                inv2.run()
            improved_avg_cost.append(np.mean(inv2.cost_log))

        avg_cost_direct.append(np.mean(direc_avg_cost))
        #avg_stock_direct.append(np.mean(inv.stock_log))
        #avg_order_direct.append(np.mean(inv.order_log))
        avg_cost_improved.append(np.mean(improved_avg_cost))
        #avg_stock_improved.append(np.mean(inv2.stock_log))
        #avg_order_improved.append(np.mean(inv2.order_log))
    print(time.time() - start)
    avg_cost_direct = np.array(avg_cost_direct)
    avg_cost_improved = np.array(avg_cost_improved)
#%%
    diff = avg_cost_direct - avg_cost_improved
    plt.close('all')
    plt.plot(avg_cost_direct[1:], label='direct')
    plt.plot(avg_cost_improved[1:], label='improved')

    plt.xticks(np.round(np.arange(0, len(ticks), 1),1),  np.round(np.arange(10, 101, 10), 1))
    plt.title('average cost to mean unit price')
    plt.xlabel('mean unit price')
    plt.ylabel('average cost')
    plt.legend()
    plt.show()
    plt.figure()
    #myticks=range(10, 91, 10)
    plt.xticks(np.round(np.arange(0, len(ticks), 1),1),  np.round(np.arange(10, 101, 10), 1))
    plt.plot(diff[1:], label='direct-improved')
    plt.legend()
    plt.title('average cost to mean unit price')
    plt.xlabel('mean unit price')
    plt.ylabel('cost different')
    plt.show()
    print("Difference between improved and direct %f" %np.mean(diff[1:]))

