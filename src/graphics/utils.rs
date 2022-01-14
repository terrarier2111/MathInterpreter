use colored::Color;
use wgpu::{Adapter, Backends, Device, DeviceDescriptor, Features, Instance, Limits, PowerPreference, PresentMode, Queue, RequestAdapterOptions, Surface, SurfaceConfiguration, SurfaceTexture, TextureUsages};
use winit::dpi::PhysicalSize;
use winit::window::Window;

pub struct ViewportDesc {
    window: Window,
    background_color: Color,
    surface: Surface,
}

pub struct Viewport {
    desc: ViewportDesc,
    config: SurfaceConfiguration,
}

impl ViewportDesc {
    pub fn new(window: Window, background_color: Color, instance: &Instance) -> Self {
        let surface = unsafe { instance.create_surface(&window) };
        Self {
            window,
            background_color,
            surface,
        }
    }

    pub fn build(self, adapter: &Adapter, device: &Device, vsync: bool) -> Viewport {
        let size = self.window.inner_size();

        let config = SurfaceConfiguration {
            usage: TextureUsages::RENDER_ATTACHMENT,
            format: self.surface.get_preferred_format(adapter).unwrap(),
            width: size.width,
            height: size.height,
            present_mode: Viewport::vsync_present_mode(vsync),
        };
        self.surface.configure(device, &config);

        Viewport { desc: self, config }
    }
}

impl Viewport {
    pub fn resize(&mut self, device: &Device, size: PhysicalSize<u32>) {
        self.config.width = size.width;
        self.config.height = size.height;
        self.desc.surface.configure(device, &self.config);
    }

    pub fn get_current_texture(&self) -> SurfaceTexture {
        self.desc
            .surface
            .get_current_texture()
            .expect("Failed to acquire next swap chain texture")
    }

    pub fn set_background_color(&mut self, background_color: Color) {
        self.desc.background_color = background_color;
    }

    pub fn set_vsync(&mut self, device: &Device, vsync: bool) {
        self.config.present_mode = Self::vsync_present_mode(vsync);
        self.desc.surface.configure(device, &self.config);
    }

    const fn vsync_present_mode(vsync: bool) -> PresentMode {
        if vsync {
            PresentMode::Fifo
        } else {
            PresentMode::Mailbox // TODO: Check if we should use PresentMode::Immediate instead!
        }
    }

    pub fn window(&self) -> &Window {
        &self.desc.window
    }

    pub fn background_color(&self) -> &Color {
        &self.desc.background_color
    }
}

pub struct GraphicsContext {
    instance: Instance,
    adapter: Adapter,
    device: Device,
    queue: Queue,
    view_port: Viewport,
}

impl GraphicsContext {

    pub async fn new(window: Window, features: Features, background_color: Color, power_preference: PowerPreference, vsync: bool) -> Self {
        let instance = Instance::new(Backends::all());
        let view_port = ViewportDesc::new(window, background_color, &instance);
        let adapter = instance
            .request_adapter(&RequestAdapterOptions {
                power_preference,
                force_fallback_adapter: false,
                // Request an adapter which can render to our surface
                compatible_surface: Some(&view_port.surface),
            })
            .await
            .expect("Failed to find an appropriate adapter");
        // Create the logical device and command queue
        let (device, queue) = adapter
            .request_device(
                &DeviceDescriptor {
                    label: None,
                    features,
                    // Make sure we use the texture resolution limits from the adapter, so we can support images the size of the swapchain.
                    limits: Limits::default()
                        .using_resolution(adapter.limits()),
                },
                None,
            )
            .await
            .expect("Failed to create device");
        let view_port = view_port.build(&adapter, &device, vsync);
        Self {
            instance,
            adapter,
            device,
            queue,
            view_port,
        }
    }

    pub fn resize(&mut self, size: PhysicalSize<u32>) {
        self.view_port.resize(&self.device, size)
    }

    pub fn set_vsync(&mut self, vsync: bool) {
        self.view_port.set_vsync(&self.device, vsync)
    }

    pub fn get_current_texture(&self) -> SurfaceTexture {
        self.view_port.get_current_texture()
    }

    pub fn window(&self) -> &Window {
        self.view_port.window()
    }

    pub fn device(&self) -> &Device {
        &self.device
    }

    pub fn background_color(&self) -> &Color {
        self.view_port.background_color()
    }
}