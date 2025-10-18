
import matplotlib.pyplot as plt
import numpy as np
import matplotlib.animation as animation
import pandas as pd

def generate_r_scale_graph():
    """
    Generates a graph showing the effect of r_scale on a noisy GPS track.
    """
    # 1. Create a "true" path (a gentle sine wave)
    t = np.linspace(0, 10, 100)
    true_path_x = t
    true_path_y = np.sin(t)

    # 2. Generate a noisy GPS track
    noise = np.random.normal(0, 0.2, 100)
    noisy_track_x = true_path_x
    noisy_track_y = true_path_y + noise

    # 3. Simulate the Kalman filter with different r_scale values
    #    (using a simple moving average as a proxy)
    def moving_average(data, window_size):
        return np.convolve(data, np.ones(window_size)/window_size, mode='valid')

    # High r_scale (more smoothing)
    high_r_scale_window = 10
    smoothed_high_r = moving_average(noisy_track_y, high_r_scale_window)

    # Low r_scale (less smoothing)
    low_r_scale_window = 3
    smoothed_low_r = moving_average(noisy_track_y, low_r_scale_window)

    # 4. Plot the results
    plt.figure(figsize=(10, 6))
    plt.plot(true_path_x, true_path_y, 'g--', label='True Path')
    plt.plot(noisy_track_x, noisy_track_y, 'k.', markersize=3, label='Noisy GPS Track')
    plt.plot(true_path_x[len(true_path_x)-len(smoothed_high_r):], smoothed_high_r, 'r-', label='Smoothed (High r_scale)')
    plt.plot(true_path_x[len(true_path_x)-len(smoothed_low_r):], smoothed_low_r, 'b-', label='Smoothed (Low r_scale)')

    plt.title("Effect of r_scale on GPS Track Smoothing")
    plt.xlabel("Longitude")
    plt.ylabel("Latitude")
    plt.legend()
    plt.grid(True)
    plt.savefig("xmldoc/images/kalman_r_scale.png")
    plt.close()
    print("Generated xmldoc/images/kalman_r_scale.png")

def generate_interpolation_graph():
    """
    Generates a graph demonstrating the interpolation of a gap in a GPS track.
    """
    # 1. Create a path with a gap
    t1 = np.linspace(0, 4, 40)
    t2 = np.linspace(6, 10, 40)
    path_x1 = t1
    path_y1 = np.sin(t1)
    path_x2 = t2
    path_y2 = np.sin(t2)

    # 2. Create the interpolated line
    interp_x = [path_x1[-1], path_x2[0]]
    interp_y = [path_y1[-1], path_y2[0]]

    # 3. Plot the results
    plt.figure(figsize=(10, 6))
    plt.plot(path_x1, path_y1, 'b-', label='Original Track')
    plt.plot(path_x2, path_y2, 'b-')
    plt.plot(interp_x, interp_y, 'r--', label='Interpolated Gap')

    plt.title("Interpolation of a Gap in a GPS Track")
    plt.xlabel("Longitude")
    plt.ylabel("Latitude")
    plt.legend()
    plt.grid(True)
    plt.savefig("xmldoc/images/kalman_interpolation.png")
    plt.close()
    print("Generated xmldoc/images/kalman_interpolation.png")

def generate_q_scale_pos_graph():
    """
    Generates both a static and an animated conceptual graph showing the effect of q_scale_pos on a sharp turn.
    """
    # 1. Create the paths
    xname="Longitude"
    yname="Latitude"
    true_path_data = pd.read_csv("xmldoc/data/kalman_q_scale_sim_output.csv", sep=",")
    zerox = true_path_data[xname][0]
    zeroy = true_path_data[yname][0]
    true_path_x = true_path_data[xname] - zerox;
    true_path_y = true_path_data[yname] - zeroy;
    low_q_data = pd.read_csv("xmldoc/data/kalman_q_scale_sim_lowq_output.csv", sep=",")
    low_q_path_x = low_q_data[xname] - zerox;
    low_q_path_y = low_q_data[yname] - zeroy;
    high_q_data = pd.read_csv("xmldoc/data/kalman_q_scale_sim_highq_output.csv", sep=",")
    high_q_path_x = high_q_data[xname] - zerox;
    high_q_path_y = high_q_data[yname] - zeroy;

    # --- Static PNG Generation ---
    fig_png, ax_png = plt.subplots(figsize=(10, 6))
    ax_png.plot(true_path_x, true_path_y, 'g--', label='True Path')
    ax_png.plot(low_q_path_x, low_q_path_y, 'r-', label='Filtered (Low q_scale_pos)')
    ax_png.plot(high_q_path_x, high_q_path_y, 'b-', label='Filtered (High q_scale_pos)')
    def add_arrows(ax, x, y, color):
        for i in range(10, len(x) - 1, 20):
            ax.arrow(x[i], y[i], x[i+1]-x[i], y[i+1]-y[i], 
                     color=color, shape='full', lw=0, length_includes_head=True, width=0, head_width=0.000015)
    add_arrows(ax_png, true_path_x, true_path_y, 'g')
    add_arrows(ax_png, low_q_path_x, low_q_path_y, 'r')
    add_arrows(ax_png, high_q_path_x, high_q_path_y, 'b')
    ax_png.set_title("Effect of q_scale_pos on Sharp Turns")
    ax_png.set_xlabel("Longitude")
    ax_png.set_ylabel("Latitude")
    ax_png.legend()
    ax_png.grid(True)
    ax_png.axis('equal')
    fig_png.savefig("xmldoc/images/kalman_q_scale_pos.png")
    plt.close(fig_png)
    print("Generated xmldoc/images/kalman_q_scale_pos.png")

    # --- Animated GIF Generation ---
    fig_gif, ax_gif = plt.subplots(figsize=(10, 6))
    ax_gif.grid(True)
    ax_gif.set_xlabel("Longitude")
    ax_gif.set_ylabel("Latitude")
    ax_gif.set_title("Effect of q_scale_pos on Sharp Turns")
    ax_gif.axis('equal')
    ax_gif.plot(true_path_x, true_path_y, 'g--', label='True Path')
    line_low_q, = ax_gif.plot([], [], 'r-', label='Filtered (Low q_scale_pos)')
    line_high_q, = ax_gif.plot([], [], 'b-', label='Filtered (High q_scale_pos)')
    marker_true, = ax_gif.plot([], [], 'go', markersize=8)
    marker_low_q, = ax_gif.plot([], [], 'ro', markersize=8)
    marker_high_q, = ax_gif.plot([], [], 'bo', markersize=8)
    ax_gif.legend()

    def animate(i):
        line_low_q.set_data(low_q_path_x[:i], low_q_path_y[:i])
        line_high_q.set_data(high_q_path_x[:i], high_q_path_y[:i])
        marker_true.set_data([true_path_x[i]], [true_path_y[i]])
        marker_low_q.set_data([low_q_path_x[i]], [low_q_path_y[i]])
        marker_high_q.set_data([high_q_path_x[i]], [high_q_path_y[i]])
        return line_low_q, line_high_q, marker_true, marker_low_q, marker_high_q

    ani = animation.FuncAnimation(fig_gif, animate, frames=len(low_q_path_x), interval=50, blit=True)
    writer = animation.PillowWriter(fps=20)
    ani.save("xmldoc/images/kalman_q_scale_pos.gif", writer=writer)
    plt.close(fig_gif)
    print("Generated xmldoc/images/kalman_q_scale_pos.gif")

def generate_max_speed_graph():
    """
    Generates a graph showing the removal of a "zinger" point.
    """
    # 1. Create a path with a zinger
    t = np.linspace(0, 10, 100)
    path_x = t
    path_y = np.sin(t)
    path_with_zinger_x = np.copy(path_x)
    path_with_zinger_y = np.copy(path_y)
    path_with_zinger_x[50:53] = [8, 8.2, 7.8]
    path_with_zinger_y[50:53] = [3, 3.1, 2.9]

    # 2. Create the path with the zinger removed
    path_without_zinger_x = np.delete(path_with_zinger_x, [50, 51, 52])
    path_without_zinger_y = np.delete(path_with_zinger_y, [50, 51, 52])

    # 3. Plot the results
    plt.figure(figsize=(10, 6))
    plt.plot(path_with_zinger_x, path_with_zinger_y, 'r-o', label='Track with Zinger Clump')
    plt.plot(path_without_zinger_x, path_without_zinger_y, 'g-o', label='Track after max_speed filter')

    plt.title("Effect of max_speed on a GPS Zinger Clump")
    plt.xlabel("Longitude")
    plt.ylabel("Latitude")
    plt.legend()
    plt.grid(True)
    plt.savefig("xmldoc/images/kalman_max_speed.png")
    plt.close()
    print("Generated xmldoc/images/kalman_max_speed.png")

if __name__ == "__main__":
    generate_r_scale_graph()
    generate_interpolation_graph()
    generate_q_scale_pos_graph()
    generate_max_speed_graph()
