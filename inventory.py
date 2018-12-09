import numpy as np
import pandas as pd
import matplotlib.pyplot as plt


class Inventory:
    def __init__(self, capacity, policy, alpha = 0.5, threshold = 50, order_max = 100, order_min = 0, init_stock=0, pre_gen_cus_order=None, pre_gen_unit_cost=None):

        self.capacity = capacity
        self.threshold = threshold
        self.order_policy = policy

        # random generated variables
        ## customer's order parameters
        self.customer_order = 0
        self.customer_order_mean = 20
        self.customer_order_std = 2
        ## unit cost of order
        self.order_u_cost = 100
        self.order_u_cost_avg = 100
        self.order_u_cost_aplha = alpha
        self.order_u_cost_noise_mean = 20
        self.order_u_cost_noise_std = 2
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
        self.order_fixed_fee = 30 # fixed cost per order
        self.actual_order_cost = 0

        # other cost constant
        self.hold_u_cost = 0.3
        self.backlog_u_cost = 20

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
            self.order_u_cost = max(0, self.order_u_cost_aplha * (self.order_u_cost_avg - self.order_u_cost) + np.random.normal(self.order_u_cost_noise_mean, self.order_u_cost_noise_std))
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

    def order_trigger(self, stock, mean_unit_cost, backlog):
        return stock * self.hold_u_cost + mean_unit_cost + self.backlog_u_cost * backlog

    def direct_refill(self):
        self.order = min(self.order_max - self.cur_stock, 100)

    def improved_refill(self):
        foo_unit_cost = np.zeros(100)
        foo_cus_order = np.zeros(100)
        foo_stock = np.zeros(100)
        foo_backlog = np.zeros(100)
        for i in range(100):
            foo_unit_cost[i] = max(0, self.order_u_cost_avg + np.random.normal(self.order_u_cost_noise_mean, self.order_u_cost_noise_std))
            foo_cus_order[i] = max(0, np.floor(np.random.normal(self.customer_order_mean, self.customer_order_std)))
            foo_stock[i] = self.cur_stock - foo_cus_order[i]
            foo_backlog[i] = abs(foo_stock[i]) if foo_stock[i] < 0 else 0

        foo_unit_cost_mean = np.mean(foo_unit_cost)
        foo_stock_mean = np.mean(foo_stock)
        foo_cus_order_mean = np.mean(foo_cus_order)
        foo_backlog_mean = np.mean(foo_backlog)
        if self.order_trigger(foo_stock_mean, foo_unit_cost_mean, foo_backlog_mean) > self.order_trigger(self.cur_stock, self.order_u_cost, 0):
            self.order = min(self.order_max - self.cur_stock, 100)
        else:
            self.order = 0

    def generate_order(self):
        'generate order for restock inventory'
        if self.cur_stock < self.threshold:
            if self.order_policy == "direct":
                self.direct_refill()

            elif self.order_policy == "improved":
                self.improved_refill()

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
    for threshold in range(10, 91):
        inv = Inventory(100, policy="direct", threshold=threshold, alpha=0.5, init_stock=100)
        for i in range(200):
            inv.run()
        avg_cost_direct.append(np.mean(inv.cost_log))
        #avg_stock_direct.append(np.mean(inv.stock_log))
        #avg_order_direct.append(np.mean(inv.order_log))

        #inv2 = Inventory(100, policy="improved", threshold=threshold, alpha=0.5, init_stock=100, \
        #                pre_gen_cus_order=inv.customer_order_log, pre_gen_unit_cost=inv.unit_cost_log)
        inv2 = Inventory(100, policy="improved", threshold=threshold, alpha=0.5, init_stock=100)
        for i in range(200):
            inv2.run()
        avg_cost_improved.append(np.mean(inv2.cost_log))
        #avg_stock_improved.append(np.mean(inv2.stock_log))
        #avg_order_improved.append(np.mean(inv2.order_log))

    avg_cost_direct = np.array(avg_cost_direct)
    avg_cost_improved = np.array(avg_cost_improved)
#%%
    diff = avg_cost_direct - avg_cost_improved
    plt.plot(avg_cost_direct, label='direct')
    plt.plot(avg_cost_improved, label='improved')
    plt.legend()
    plt.figure()
    plt.plot(diff, label='direct-improved')
    plt.legend()
    print(np.mean(diff))

